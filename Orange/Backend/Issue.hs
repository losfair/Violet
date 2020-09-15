module Orange.Backend.Issue where

import Clash.Prelude
import Data.Maybe
import Orange.Types.Issue
import qualified Orange.Types.Fifo as FifoT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Pipe as PipeT
import qualified Orange.Types.Memory as MemoryT
import qualified Orange.Types.Branch as BranchT
import qualified Orange.Types.DecodeDep as DepT
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Ctrl as CtrlT
import qualified Debug.Trace
import qualified Prelude

data IssueState = IssueState {
    loadActivated :: BitVector 2,
    ctrlActivated :: BitVector 2
} deriving (Generic, NFDataX)

type IssueInput = (FifoT.FifoItem, FifoT.FifoItem)
type IssueOutput = (((IssuePort, IssuePort), ActivationMask), PipeT.Recovery, PipeT.Recovery, FifoT.FifoPopReq)

issue' :: (IssueState, IssuePort, IssuePort, ActivationMask, (PipeT.Recovery, FifoT.FifoPopReq))
       -> IssueInput
       -> ((IssueState, IssuePort, IssuePort, ActivationMask, (PipeT.Recovery, FifoT.FifoPopReq)), IssueOutput)
issue' (state, port1, port2, am, (recovery, popReq)) (item1, item2) =
    ((state', port1', port2', am', (recovery', popReq')), (((port1, port2), am), recovery, recovery', popReq))
    where
        -- item1 = Debug.Trace.trace ("Item1: " Prelude.++ show item1_) item1_
        -- item2 = Debug.Trace.trace ("Item2: " Prelude.++ show item2_) item2_
        wantsLoadAccess = itemWantsLoadAccess item1
        wantsCtrlAccess = itemWantsCtrlAccess item1
        exceptionResolvedAt1 = itemWantsExceptionResolution item1
        exceptionResolvedAt2 = itemWantsExceptionResolution item2
        isExceptionResolved = exceptionResolvedAt1 || exceptionResolvedAt2

        -- Port 1 should be disabled if exception is resolved at port 2
        disableFirstPort = exceptionResolvedAt2

        loadActivated_ = loadActivated state /= 0
        ctrlActivated_ = ctrlActivated state /= 0

        pipelineBlocked = loadActivated_ || ctrlActivated_

        -- Don't re-activate if we didn't issue
        loadActivated' = slice d0 d0 (loadActivated state) ++# if wantsLoadAccess && not pipelineBlocked then 0b1 else 0b0
        ctrlActivated' = slice d0 d0 (ctrlActivated state) ++# if wantsCtrlAccess && not pipelineBlocked then 0b1 else 0b0

        amNormal = ActivationMask {
            amInt1 = lookActivation item1 DepT.actInt && not disableFirstPort,
            amInt2 = lookActivation item2 DepT.actInt && canConcurrentIssue item2,
            amBranch = lookActivation item1 DepT.actBranch && not disableFirstPort,
            amMem = lookActivation item1 (\x -> DepT.actLoad x || DepT.actStore x) && not disableFirstPort,
            amCtrl =
                if lookActivation item1 DepT.actException && not disableFirstPort then Just CtrlDecodeException
                else if lookActivation item1 DepT.actCtrl && not disableFirstPort then Just CtrlNormal
                else Nothing
        }
        am' = if pipelineBlocked then emptyActivationMask else amNormal
        popReq' = if pipelineBlocked || FifoT.isBubble item1 then FifoT.PopNothing
                    else if not (canConcurrentIssue item2) || FifoT.isBubble item2 then FifoT.PopOne
                    else FifoT.PopTwo
        recovery' = if isExceptionResolved then PipeT.IsRecovery else PipeT.NotRecovery
        port1' = genIssuePort item1
        port2' = genIssuePort item2
        state' = IssueState { loadActivated = loadActivated', ctrlActivated = ctrlActivated' }

issue :: HiddenClockResetEnable dom
      => Signal dom IssueInput
      -> Signal dom IssueOutput
issue = mealy issue' (IssueState { loadActivated = 0, ctrlActivated = 0 }, emptyIssuePort, emptyIssuePort, emptyActivationMask, (PipeT.NotRecovery, FifoT.PopNothing))

itemWantsLoadAccess :: FifoT.FifoItem -> Bool
itemWantsLoadAccess item = case item of
    FifoT.Item (_, _, _, act, _, _, _) -> DepT.actLoad act
    _ -> False

itemWantsCtrlAccess :: FifoT.FifoItem -> Bool
itemWantsCtrlAccess item = case item of
    FifoT.Item (_, _, _, act, _, _, _) -> DepT.actCtrl act
    _ -> False

itemWantsExceptionResolution :: FifoT.FifoItem -> Bool
itemWantsExceptionResolution item = case item of
    FifoT.Item (_, _, md, _, _, _, _) -> FetchT.exceptionResolved md
    _ -> False

canConcurrentIssue :: FifoT.FifoItem -> Bool
canConcurrentIssue item = case item of
    FifoT.Item (_, _, _, _, _, conc, _) -> conc == DepT.CanConcurrentIssue
    _ -> True

lookInside :: FifoT.FifoItem -> (DepT.DecodeDepBundle -> Bool) -> Bool
lookInside item f = case item of
    FifoT.Item x -> f x
    _ -> False

lookActivation :: FifoT.FifoItem -> (DepT.Activation -> Bool) -> Bool
lookActivation item f = lookInside item (\(_, _, _, x, _, _, _) -> f x)

lookPCAssumeItem :: FifoT.FifoItem -> FetchT.PC
lookPCAssumeItem item = case item of
    FifoT.Item (pc, _, _, _, _, _, _) -> pc
    _ -> undefined -- impossible by contract

genIssuePort :: FifoT.FifoItem -> IssuePort
genIssuePort item = case item of
    FifoT.Item (pc, inst, md, _, _, _, _) -> (pc, inst, md)
    _ -> emptyIssuePort

emptyIssuePort :: IssuePort
emptyIssuePort = (0, FetchT.nopInst, FetchT.emptyMetadata)

emptyActivationMask :: ActivationMask
emptyActivationMask = ActivationMask {
    amInt1 = False,
    amInt2 = False,
    amBranch = False,
    amMem = False,
    amCtrl = Nothing
}