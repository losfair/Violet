module Violet.Backend.Issue where

import Clash.Prelude
import Data.Maybe
import Violet.Types.Issue
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Pipe as PipeT
import qualified Violet.Types.Memory as MemoryT
import qualified Violet.Types.Branch as BranchT
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Ctrl as CtrlT
import qualified Debug.Trace
import qualified Prelude

data IssueState = IssueState {
    loadActivated :: Vec 2 (Maybe GprT.RegIndex),
    ctrlFirstCycle :: Bool
} deriving (Generic, NFDataX)

type IssueInput = (FifoT.FifoItem, FifoT.FifoItem, CtrlT.CtrlBusy)
type IssueOutput = (((IssuePort, IssuePort), ActivationMask), PipeT.Recovery, PipeT.Recovery, FifoT.FifoPopReq)

data Activation = Activation {
    actInt :: Bool,
    actBranch :: Bool,
    actLoad :: Bool,
    actStore :: Bool,
    actCtrl :: Bool,
    actException :: Bool
} deriving (Generic, NFDataX, Show)

data Concurrency = CanConcurrentIssue | NoConcurrentIssue
    deriving (Generic, NFDataX, Eq, Show)

data RegLayout = RegLayout {
    hasRd :: Bool,
    hasRs1 :: Bool,
    hasRs2 :: Bool
} deriving (Generic, NFDataX, Eq, Show)

layoutRd = RegLayout { hasRd = True, hasRs1 = False, hasRs2 = False }
layoutRdRs1 = RegLayout { hasRd = True, hasRs1 = True, hasRs2 = False }
layoutRdRs1Rs2 = RegLayout { hasRd = True, hasRs1 = True, hasRs2 = True }
layoutRs1Rs2 = RegLayout { hasRd = False, hasRs1 = True, hasRs2 = True }
layoutNoReg = RegLayout { hasRd = False, hasRs1 = False, hasRs2 = False }

decode :: FetchT.Inst -> (Activation, RegLayout)
decode inst =
    if inst == FetchT.nopInst then
        (emptyActivation, layoutNoReg)
    else case slice d6 d0 inst of
        0b0110111 -> (intActivation, layoutRd) -- lui
        0b0010111 -> (intActivation, layoutRd) -- auipc
        0b1101111 -> (jalActivation, layoutRd) -- jal
        0b1100111 -> (jalActivation, layoutRdRs1) -- jalr
        0b1100011 -> (jActivation, layoutRs1Rs2) -- beq/bne/blt/bge/bltu/bgeu
        0b0000011 -> (loadActivation, layoutRdRs1) -- load
        0b0100011 -> (storeActivation, layoutRs1Rs2) -- store
        0b0010011 -> (intActivation, layoutRdRs1) -- ALU with imm
        0b0110011 -> case slice d31 d25 inst of
            0b0000001 -> case slice d14 d14 inst of
                0b0 -> (ctrlActivation, layoutRdRs1Rs2) -- mul
                0b1 -> (ctrlActivation, layoutRdRs1Rs2) -- div
            _ -> (intActivation, layoutRdRs1Rs2)
        0b0001111 -> (ctrlActivation, layoutRdRs1) -- fence
        0b1110011 -> (ctrlActivation, layoutNoReg) -- ecall/ebreak
        _ -> (excActivation, layoutNoReg)

dep :: (FetchT.Inst, Activation, RegLayout) -> (FetchT.Inst, Activation, RegLayout) -> Concurrency
dep (inst1, act1, layout1) (inst2, act2, layout2) = if anyHazard then NoConcurrentIssue else CanConcurrentIssue
    where
        memConflict = actLoad act2 || actStore act2 -- port 1 only
        ctrlConflict = actCtrl act1 || actCtrl act2
        exceptionConflict = actException act1 || actException act2
        structuralHazard = memConflict || ctrlConflict || exceptionConflict

        inst1Rs1 = slice d19 d15 inst1
        inst1Rs2 = slice d24 d20 inst1
        inst1Rd = slice d11 d7 inst1

        inst2Rs1 = slice d19 d15 inst2
        inst2Rs2 = slice d24 d20 inst2
        inst2Rd = slice d11 d7 inst2

        inst1WritesRd = hasRd layout1
        inst2ReadsRs1 = hasRs1 layout2
        inst2ReadsRs2 = hasRs2 layout2
        regConflictCase1 = inst1WritesRd && inst2ReadsRs1 && inst1Rd == inst2Rs1 && inst1Rd /= 0
        regConflictCase2 = inst1WritesRd && inst2ReadsRs2 && inst1Rd == inst2Rs2 && inst1Rd /= 0
        regHazard = regConflictCase1 || regConflictCase2

        anyHazard = structuralHazard || regHazard

decodeDep' :: FetchT.Inst
           -> FetchT.Inst
           -> ((Activation, RegLayout), (Activation, RegLayout), Concurrency)
decodeDep' a b = ((act1, layout1), (act2, layout2), conc)
    where
        (act1, layout1) = decode a
        (act2, layout2) = decode b
        conc = dep (a, act1, layout1) (b, act2, layout2)

decodeDep :: FifoT.FifoItem
          -> FifoT.FifoItem
          -> ((Activation, RegLayout), (Activation, RegLayout), Concurrency)
decodeDep a b = decodeDep' inst1 inst2
    where
        inst1 = selInst a
        inst2 = selInst b
        selInst (FifoT.Item (_, x, md)) = if FetchT.isValidInst md then x else FetchT.nopInst
        selInst FifoT.Bubble = FetchT.nopInst

issue' :: (IssueState, IssuePort, IssuePort, ActivationMask, (PipeT.Recovery, FifoT.FifoPopReq))
       -> IssueInput
       -> ((IssueState, IssuePort, IssuePort, ActivationMask, (PipeT.Recovery, FifoT.FifoPopReq)), IssueOutput)
issue' (state, port1, port2, am, (recovery, popReq)) (item1, item2, ctrlBusy) =
    ((state', port1', port2', am', (recovery', popReq')), (((port1, port2), am), recovery, recovery', popReq))
    where
        -- item1 = Debug.Trace.trace ("Item1: " Prelude.++ show item1_) item1_
        -- item2 = Debug.Trace.trace ("Item2: " Prelude.++ show item2_) item2_
        ((act1, layout1), (act2, layout2), concurrency) = decodeDep item1 item2
        exceptionResolvedAt1 = itemWantsExceptionResolution item1
        exceptionResolvedAt2 = itemWantsExceptionResolution item2
        isExceptionResolved = exceptionResolvedAt1 || exceptionResolvedAt2

        -- Port 1 should be disabled if exception is resolved at port 2
        disableFirstPort = exceptionResolvedAt2
        enableSecondPort = concurrency == CanConcurrentIssue

        -- Load blocked if a load-use hazard is detected or we are resolving an exception.
        -- We need to block on exception resolution because the DCache pipeline is separate from
        -- the main commit pipelines and will not get reset on recovery signals.
        loadBlocked =
            (not disableFirstPort && hasLoadUse state item1 layout1) ||
            (enableSecondPort && hasLoadUse state item2 layout2) ||
            (isExceptionResolved && hasOngoingLoad state)
        ctrlBlocked = ctrlFirstCycle state || ctrlBusy == CtrlT.Busy

        pipelineBlocked = loadBlocked || ctrlBlocked

        -- Don't re-activate if we didn't issue
        loadDst = if not pipelineBlocked then itemLoadDstReg item1 act1 else Nothing
        loadActivated' = fst $ shiftInAtN (loadActivated state) (loadDst :> Nil)
        ctrlFirstCycle' = actCtrl act1 && not pipelineBlocked

        amNormal = ActivationMask {
            amInt1 = actInt act1 && not disableFirstPort,
            amInt2 = actInt act2 && enableSecondPort,
            amBranch1 = actBranch act1 && not disableFirstPort,
            amBranch2 = actBranch act2 && enableSecondPort,
            amMem = (actLoad act1 || actStore act1) && not disableFirstPort,
            amCtrl =
                if actException act1 && not disableFirstPort then Just CtrlDecodeException
                else if actCtrl act1 && not disableFirstPort then Just CtrlNormal
                else Nothing
        }
        am' = if pipelineBlocked then emptyActivationMask else amNormal
        popReq' = if pipelineBlocked || FifoT.isBubble item1 then FifoT.PopNothing
                    else if not enableSecondPort || FifoT.isBubble item2 then FifoT.PopOne
                    else FifoT.PopTwo
        recovery' = if isExceptionResolved then PipeT.IsRecovery else PipeT.NotRecovery
        port1' = genIssuePort item1
        port2' = genIssuePort item2
        state' = IssueState { loadActivated = loadActivated', ctrlFirstCycle = ctrlFirstCycle' }

issue :: HiddenClockResetEnable dom
      => Signal dom IssueInput
      -> Signal dom IssueOutput
issue = mealy issue' (IssueState { loadActivated = repeat Nothing, ctrlFirstCycle = False }, emptyIssuePort, emptyIssuePort, emptyActivationMask, (PipeT.NotRecovery, FifoT.PopNothing))

hasOngoingLoad :: IssueState -> Bool
hasOngoingLoad state = case findIndex (\x -> x /= Nothing) (loadActivated state) of
    Just _ -> True
    Nothing -> False

hasLoadUse :: IssueState -> FifoT.FifoItem -> RegLayout -> Bool
hasLoadUse state item layout = case item of
    FifoT.Item (_, i, _) ->
        let
            (rs1, rs2) = GprT.decodeRs i
            in
                case findIndex (\x -> (hasRs1 layout && x == Just rs1) || (hasRs2 layout && x == Just rs2)) (loadActivated state) of
                    Just _ -> True
                    Nothing -> False
    _ -> False

itemLoadDstReg :: FifoT.FifoItem -> Activation -> Maybe GprT.RegIndex
itemLoadDstReg item act = case item of
    FifoT.Item (_, i, _) ->
        if actLoad act then
            case GprT.decodeRd i of
                0 -> Nothing
                x -> Just x
        else Nothing
    _ -> Nothing

itemWantsExceptionResolution :: FifoT.FifoItem -> Bool
itemWantsExceptionResolution item = case item of
    FifoT.Item (_, _, md) -> FetchT.exceptionResolved md
    _ -> False

genIssuePort :: FifoT.FifoItem -> IssuePort
genIssuePort item = case item of
    FifoT.Item (pc, inst, md) -> (pc, inst, md)
    _ -> emptyIssuePort

emptyIssuePort :: IssuePort
emptyIssuePort = (0, FetchT.nopInst, FetchT.emptyMetadata)

emptyActivationMask :: ActivationMask
emptyActivationMask = ActivationMask {
    amInt1 = False,
    amInt2 = False,
    amBranch1 = False,
    amBranch2 = False,
    amMem = False,
    amCtrl = Nothing
}

emptyActivation :: Activation
emptyActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = False
}

intActivation :: Activation
intActivation = Activation {
    actInt = True,
    actBranch = False,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = False
}

jalActivation :: Activation
jalActivation = Activation {
    actInt = True,
    actBranch = True,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = False
}

jActivation :: Activation
jActivation = Activation {
    actInt = False,
    actBranch = True,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = False
}

loadActivation :: Activation
loadActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = True,
    actStore = False,
    actCtrl = False,
    actException = False
}

storeActivation :: Activation
storeActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = False,
    actStore = True,
    actCtrl = False,
    actException = False
}

ctrlActivation :: Activation
ctrlActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = False,
    actStore = False,
    actCtrl = True,
    actException = False
}

excActivation :: Activation
excActivation = Activation {
    actInt = False,
    actBranch = False,
    actLoad = False,
    actStore = False,
    actCtrl = False,
    actException = True
}
