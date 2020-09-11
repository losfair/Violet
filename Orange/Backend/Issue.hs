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

data IssueState = IssueState {
    loadActivated :: Bool,
    ctrlActivated :: Bool
} deriving (Generic, NFDataX)

data ActivationMask = ActivationMask {
    amInt1 :: Bool,
    amInt2 :: Bool,
    amBranch :: Bool,
    amMem :: Bool,
    amCtrl :: Bool
} deriving (Generic, NFDataX)

type IssueInput = (FifoT.FifoItem, FifoT.FifoItem, CtrlT.CtrlAck, MemoryT.MemoryAck)
type IssueOutput = (FunctionUnitActivation, PipeT.Commit, PipeT.Recovery, FifoT.FifoPopReq)

issue' :: (IssueState, IssuePort, IssuePort, ActivationMask, (PipeT.Commit, PipeT.Recovery, FifoT.FifoPopReq))
       -> IssueInput
       -> ((IssueState, IssuePort, IssuePort, ActivationMask, (PipeT.Commit, PipeT.Recovery, FifoT.FifoPopReq)), IssueOutput)
issue' (state, port1, port2, am, (commit, recovery, popReq)) (item1, item2, ctrlAck, memAck) =
    ((state', port1', port2', am', (commit', recovery', popReq')), (maskActivation am port1 port2, commit, recovery, popReq))
    where
        wantsLoadAccess = itemWantsLoadAccess item1
        wantsCtrlAccess = itemWantsCtrlAccess item1
        isMispredictionResolved = itemWantsMispredictionResolution item1

        loadActivated_ = loadActivated state && memAck /= MemoryT.MemAck
        loadActivated' = loadActivated_ || wantsLoadAccess

        ctrlActivated_ = ctrlActivated state && ctrlAck /= CtrlT.CtrlAck
        ctrlActivated' = ctrlActivated_ || wantsCtrlAccess

        pipelineBlocked = loadActivated_ || ctrlActivated_

        amNormal = ActivationMask {
            amInt1 = lookActivation item1 DepT.actInt,
            amInt2 = lookActivation item2 DepT.actInt && canConcurrentIssue item2,
            amBranch = lookActivation item1 DepT.actBranch,
            amMem = lookActivation item1 (\x -> DepT.actLoad x || DepT.actStore x),
            amCtrl = lookActivation item1 DepT.actCtrl
        }
        am' = if pipelineBlocked then emptyActivationMask else amNormal
        popReq' = if pipelineBlocked then FifoT.PopNothing else if canConcurrentIssue item2 then FifoT.PopTwo else FifoT.PopOne
        recovery' = if isMispredictionResolved then PipeT.IsRecovery else PipeT.NotRecovery
        commit' = if lookActivation item1 DepT.actException then PipeT.Exc (lookPCAssumeItem item1, PipeT.DecodeExc) else PipeT.Bubble
        port1' = genIssuePort item1
        port2' = genIssuePort item2
        state' = IssueState { loadActivated = loadActivated', ctrlActivated = ctrlActivated' }

issue :: HiddenClockResetEnable dom
      => Signal dom IssueInput
      -> Signal dom IssueOutput
issue = mealy issue' (IssueState { loadActivated = False, ctrlActivated = False }, emptyIssuePort, emptyIssuePort, emptyActivationMask, (PipeT.Bubble, PipeT.NotRecovery, FifoT.PopNothing))

maskActivation :: ActivationMask -> IssuePort -> IssuePort -> FunctionUnitActivation
maskActivation mask port1 port2 = FunctionUnitActivation {
    fuInt1 = if amInt1 mask then Just port1 else Nothing,
    fuInt2 = if amInt2 mask then Just port2 else Nothing,
    fuBranch = if amBranch mask then Just port1 else Nothing,
    fuMem = if amMem mask then Just port1 else Nothing,
    fuCtrl = if amCtrl mask then Just port1 else Nothing
}

itemWantsLoadAccess :: FifoT.FifoItem -> Bool
itemWantsLoadAccess item = case item of
    FifoT.Item (_, _, _, act, _, _, _) -> DepT.actLoad act
    _ -> False

itemWantsCtrlAccess :: FifoT.FifoItem -> Bool
itemWantsCtrlAccess item = case item of
    FifoT.Item (_, _, _, act, _, _, _) -> DepT.actCtrl act
    _ -> False

itemWantsMispredictionResolution :: FifoT.FifoItem -> Bool
itemWantsMispredictionResolution item = case item of
    FifoT.Item (_, _, md, _, _, _, _) -> FetchT.branchMispredictionResolved md
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
    amCtrl = False
}