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
import qualified Violet.Config as Config

data LateActivationType = NoLate | MemLoad GprT.RegIndex | LateAlu GprT.RegIndex
    deriving (Generic, NFDataX, Eq)

data IssueState = IssueState {
    lateActivated1 :: Vec 2 LateActivationType,
    lateActivated2 :: Vec 2 LateActivationType,
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

decode :: (FetchT.Inst, FetchT.Metadata) -> (Activation, RegLayout)

-- This requires `FetchT.isValidInst == True`: IC miss is still a "valid instruction", although a
-- special one.
decode (_, md) | FetchT.icMiss md = (ctrlActivation, layoutNoReg)

decode (inst, _) = case slice d6 d0 inst of
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
            0b0 -> -- mul
                if Config.mulInAlu && slice d13 d12 inst == 0 then
                    (intActivation, layoutRdRs1Rs2)
                else
                    (ctrlActivation, layoutRdRs1Rs2)
            0b1 -> (ctrlActivation, layoutRdRs1Rs2) -- div
        _ -> (intActivation, layoutRdRs1Rs2)
    0b0001111 -> (ctrlActivation, layoutRdRs1) -- fence
    0b1110011 -> (ctrlActivation, layoutNoReg) -- ecall/ebreak
    _ -> (excActivation, layoutNoReg)

dep :: ((FetchT.Inst, FetchT.Metadata), Activation, RegLayout) -> ((FetchT.Inst, FetchT.Metadata), Activation, RegLayout) -> Concurrency
dep ((inst1, _), act1, layout1) ((inst2, _), act2, layout2) = if anyHazard then NoConcurrentIssue else CanConcurrentIssue
    where
        memConflict = actStore act2 || actLoad act2 -- load/store can only issue on port 1
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

decodeDep' :: (FetchT.Inst, FetchT.Metadata)
           -> (FetchT.Inst, FetchT.Metadata)
           -> ((Activation, RegLayout), (Activation, RegLayout), Concurrency)
decodeDep' a b = ((act1, layout1), (act2, layout2), conc)
    where
        (act1, layout1) = decode a
        (act2, layout2) = decode b
        conc = dep (a, act1, layout1) (b, act2, layout2)

decodeDep :: FifoT.FifoItem
          -> FifoT.FifoItem
          -> ((Activation, RegLayout), (Activation, RegLayout), Concurrency)
decodeDep a b = case (inst1, inst2) of
    (Just ll, Just rr) -> decodeDep' ll rr
    (Just ll, Nothing) -> (decode ll, (emptyActivation, layoutNoReg), CanConcurrentIssue)
    (Nothing, Just rr) -> ((emptyActivation, layoutNoReg), decode rr, CanConcurrentIssue)
    (Nothing, Nothing) -> ((emptyActivation, layoutNoReg), (emptyActivation, layoutNoReg), CanConcurrentIssue)
    where
        inst1 = selInst a
        inst2 = selInst b
        selInst (FifoT.Item (_, x, md)) = if FetchT.isValidInst md then Just (x, md) else Nothing
        selInst FifoT.Bubble = Nothing

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

        useConflict1 = hasUseConflict state item1 layout1
        useConflict2 = hasUseConflict state item2 layout2

        -- See whether this is a deferrable ALU (int/branch) instruction.
        aluDeferred1 = Config.lateALU && not disableFirstPort && useConflict1 && (actInt act1 || actBranch act1)
        aluDeferred2 = Config.lateALU && enableSecondPort && useConflict2 && (actInt act2 || actBranch act2)

        -- Data blocked if a load/latealu-use hazard is detected or we are resolving an exception.
        -- We need to block on exception resolution because the DCache pipeline is separate from
        -- the main commit pipelines and will not get reset on recovery signals.
        dataBlocked =
            (not disableFirstPort && useConflict1 && not aluDeferred1) ||
            (enableSecondPort && useConflict2 && not aluDeferred2) ||
            (isExceptionResolved && hasOngoingLoadOrLateAlu state)
        ctrlBlocked = ctrlFirstCycle state || ctrlBusy == CtrlT.Busy

        pipelineBlocked = dataBlocked || ctrlBlocked

        -- Don't re-activate if we didn't issue
        lateDst1 = if not pipelineBlocked && not disableFirstPort then resolveLateActivationType item1 act1 layout1 aluDeferred1 else NoLate
        lateDst2 = if not pipelineBlocked && enableSecondPort then resolveLateActivationType item2 act2 layout2 aluDeferred2 else NoLate
        lateActivated1' = fst $ shiftInAtN (lateActivated1 state) (lateDst1 :> Nil)
        lateActivated2' = fst $ shiftInAtN (lateActivated2 state) (lateDst2 :> Nil)
        ctrlFirstCycle' = actCtrl act1 && not pipelineBlocked

        amNormal = ActivationMask {
            amInt1 = actInt act1 && not disableFirstPort && not aluDeferred1,
            amInt2 = actInt act2 && enableSecondPort && not aluDeferred2,
            amBranch1 = actBranch act1 && not disableFirstPort && not aluDeferred1,
            amBranch2 = actBranch act2 && enableSecondPort && not aluDeferred2,
            amLateInt1 = actInt act1 && not disableFirstPort && aluDeferred1,
            amLateInt2 = actInt act2 && enableSecondPort && aluDeferred2,
            amLateBranch1 = actBranch act1 && not disableFirstPort && aluDeferred1,
            amLateBranch2 = actBranch act2 && enableSecondPort && aluDeferred2,
            amMem1 = (actLoad act1 || actStore act1) && not disableFirstPort,
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
        state' = IssueState { lateActivated1 = lateActivated1', lateActivated2 = lateActivated2', ctrlFirstCycle = ctrlFirstCycle' }

issue :: HiddenClockResetEnable dom
      => Signal dom IssueInput
      -> Signal dom IssueOutput
issue = mealy issue' (IssueState { lateActivated1 = repeat NoLate, lateActivated2 = repeat NoLate, ctrlFirstCycle = False }, emptyIssuePort, emptyIssuePort, emptyActivationMask, (PipeT.NotRecovery, FifoT.PopNothing))

hasOngoingLoadOrLateAlu :: IssueState -> Bool
hasOngoingLoadOrLateAlu state = f (lateActivated1 state) || f (lateActivated2 state)
    where
        f actList = case findIndex (\x -> x /= NoLate) actList of
            Just _ -> True
            Nothing -> False

hasUseConflict :: IssueState -> FifoT.FifoItem -> RegLayout -> Bool
hasUseConflict state item layout = case item of
    FifoT.Item (_, i, _) ->
        f (lateActivated1 state) || f (lateActivated2 state)
            where
                (rs1, rs2) = GprT.decodeRs i
                f actList = case findIndex (\x -> (hasRs1 layout && (x == MemLoad rs1 || x == LateAlu rs1)) || (hasRs2 layout && (x == MemLoad rs2 || x == LateAlu rs2))) actList of
                    Just _ -> True
                    Nothing -> False
    _ -> False

resolveLateActivationType :: FifoT.FifoItem -> Activation -> RegLayout -> Bool -> LateActivationType
resolveLateActivationType item act layout aluDeferred = case item of
    FifoT.Item (_, i, _) ->
        if actLoad act then
            case GprT.decodeRd i of
                0 -> NoLate
                x -> MemLoad x
        else if hasRd layout && (actInt act || actBranch act) && aluDeferred then
            case GprT.decodeRd i of
                0 -> NoLate
                x -> LateAlu x
        else NoLate
    _ -> NoLate

itemWantsExceptionResolution :: FifoT.FifoItem -> Bool
itemWantsExceptionResolution item = case item of
    FifoT.Item (_, _, md) -> FetchT.exceptionResolved md
    _ -> False

genIssuePort :: FifoT.FifoItem -> IssuePort
genIssuePort item = case item of
    FifoT.Item (pc, inst, md) -> (pc, inst, md)
    _ -> emptyIssuePort

emptyIssuePort :: IssuePort
emptyIssuePort = (0, undefined, FetchT.emptyMetadata)

emptyActivationMask :: ActivationMask
emptyActivationMask = ActivationMask {
    amInt1 = False,
    amInt2 = False,
    amBranch1 = False,
    amBranch2 = False,
    amLateInt1 = False,
    amLateInt2 = False,
    amLateBranch1 = False,
    amLateBranch2 = False,
    amMem1 = False,
    amCtrl = Nothing
}

emptyBypassInput = ((emptyIssuePort, emptyIssuePort), emptyActivationMask)

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
