module Orange.Backend.Wiring where

import Clash.Prelude

import qualified Orange.Backend.Bypass
import qualified Orange.Backend.Gpr
import qualified Orange.Backend.Branch
import qualified Orange.Backend.Commit
import qualified Orange.Backend.DCache
import qualified Orange.Backend.IntAlu
import qualified Orange.Backend.Issue
import qualified Orange.Backend.Pipe
import qualified Orange.Backend.Fifo
import qualified Orange.Backend.Ctrl

import qualified Orange.Types.Fifo as FifoT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Pipe as PipeT
import qualified Orange.Types.Memory as MemoryT
import qualified Orange.Types.Branch as BranchT
import qualified Orange.Types.DecodeDep as DepT
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Ctrl as CtrlT
import qualified Orange.Types.Issue as IssueT
import qualified Orange.Types.Commit as CommitT
import qualified Orange.Types.DCache as DCacheT

wiring :: HiddenClockResetEnable dom
       => DCacheT.DCacheImpl a
       => a
       -> Signal dom (FifoT.FifoItem, FifoT.FifoItem)
       -> Signal dom (FetchT.BackendCmd, CommitT.CommitLog, FifoT.FifoPushCap)
wiring dcacheImpl frontPush = bundle $ (backendCmd, commitLog, fifoPushCap)
    where
        (frontPush1, frontPush2) = unbundle frontPush
        (issueInput1, issueInput2, fifoPushCap) = unbundle $ Orange.Backend.Fifo.fifo $ bundle (frontPush1, frontPush2, fifoPopReq)
        (bypassInput, recovery, immRecovery, fifoPopReq) = unbundle $ Orange.Backend.Issue.issue $ bundle (issueInput1, issueInput2, ctrlBusy)
        gprFetch = Orange.Backend.Gpr.gpr $ bundle (bundle (issueInput1, issueInput2), gprWritePort1, gprWritePort2)
        (fuActivation, gprPort1, gprPort2) = Orange.Backend.Bypass.bypass bypassInput gprFetch commitPipe1 commitPipe2
        commitPipe1 = Orange.Backend.Pipe.completionPipe immRecovery commitStagesIn1
        commitPipe2 = Orange.Backend.Pipe.completionPipe immRecovery commitStagesIn2
        recoveryPipe = Orange.Backend.Pipe.recoveryPipe recoveryStagesIn
        intAlu1 = Orange.Backend.IntAlu.intAlu (fmap IssueT.fuInt1 fuActivation) gprPort1
        intAlu2 = Orange.Backend.IntAlu.intAlu (fmap IssueT.fuInt2 fuActivation) gprPort2
        branchUnit = Orange.Backend.Branch.branch (fmap IssueT.fuBranch fuActivation) gprPort1
        (dcacheUnit, dcWeReq) = Orange.Backend.DCache.dcache dcacheImpl (fmap IssueT.fuMem fuActivation) gprPort1 dcWeCommit
        (ctrlUnit, ctrlBusy) = unbundle $ Orange.Backend.Ctrl.ctrl (fmap IssueT.fuCtrl fuActivation) gprPort1

        (gprWritePort1, gprWritePort2, backendCmd, dcWeCommit, commitLog) = unbundle $ Orange.Backend.Commit.commit $ bundle (last commitPipe1, last commitPipe2, last recoveryPipe, dcWeReq)
        commitStagesIn1 =
            selectCommit (selectCommit intAlu1 branchUnit) ctrlUnit
            :> commitPipe1 !! 0
            :> selectCommit (commitPipe1 !! 1) dcacheUnit
            :> Nil
        commitStagesIn2 =
            intAlu2
            :> commitPipe2 !! 0
            :> commitPipe2 !! 1
            :> Nil
        recoveryStagesIn =
            recovery
            :> recoveryPipe !! 0
            :> recoveryPipe !! 1
            :> Nil

selectCommit :: HiddenClockResetEnable dom
             => Signal dom PipeT.Commit
             -> Signal dom PipeT.Commit
             -> Signal dom PipeT.Commit
selectCommit a b = fmap f $ bundle (a, b)
    where
        f (a, b) = case (a, b) of
            (left, PipeT.Bubble) -> left
            (_, right) -> right