module Violet.Backend.Wiring where

import Clash.Prelude

import qualified Violet.Backend.Bypass
import qualified Violet.Backend.Gpr
import qualified Violet.Backend.Branch
import qualified Violet.Backend.Commit
import qualified Violet.Backend.DCache
import qualified Violet.Backend.IntAlu
import qualified Violet.Backend.Issue
import qualified Violet.Backend.Pipe
import qualified Violet.Backend.Fifo
import qualified Violet.Backend.Ctrl
import qualified Violet.Backend.PerfCounter

import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Pipe as PipeT
import qualified Violet.Types.Memory as MemoryT
import qualified Violet.Types.Branch as BranchT
import qualified Violet.Types.DecodeDep as DepT
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Ctrl as CtrlT
import qualified Violet.Types.Issue as IssueT
import qualified Violet.Types.Commit as CommitT
import qualified Violet.Types.DCache as DCacheT

import qualified Violet.Config as Config

wiring :: HiddenClockResetEnable dom
       => DCacheT.DCacheImpl a
       => a
       -> Signal dom (FifoT.FifoItem, FifoT.FifoItem)
       -> Signal dom CtrlT.SystemBusIn
       -> Signal dom (FetchT.BackendCmd, CommitT.CommitLog, FifoT.FifoPushCap, CtrlT.SystemBusOut, Maybe FetchT.HistoryUpdate, CtrlT.IcRefillIn)
wiring dcacheImpl frontPush sysIn = bundle $ (backendCmd, commitLog, fifoPushCap, sysOut, historyUpd, icRefillIn)
    where
        (frontPush1, frontPush2) = unbundle frontPush
        (issueInput1, issueInput2, fifoPushCap) = unbundle $ Violet.Backend.Fifo.fifo $ bundle (frontPush1, frontPush2, fifoPopReq)
        (bypassInput, recovery, immRecovery, fifoPopReq) = unbundle $ Violet.Backend.Issue.issue $ bundle (issueInput1, issueInput2, ctrlBusy)
        gprFetch = Violet.Backend.Gpr.gpr $ bundle (bundle (issueInput1, issueInput2), gprWritePort1, gprWritePort2)
        (fuActivation, gprPort1, gprPort2) = Violet.Backend.Bypass.bypass bypassInput gprFetch commitPipe1 commitPipe2
        commitPipe1 = Violet.Backend.Pipe.completionPipe immRecovery commitStagesIn1
        commitPipe2 = Violet.Backend.Pipe.completionPipe immRecovery commitStagesIn2
        recoveryPipe = Violet.Backend.Pipe.recoveryPipe recoveryStagesIn
        intAlu1 = Violet.Backend.IntAlu.intAlu (fmap IssueT.fuInt1 fuActivation) gprPort1
        intAlu2 = Violet.Backend.IntAlu.intAlu (fmap IssueT.fuInt2 fuActivation) gprPort2
        branchUnit1 = Violet.Backend.Branch.branch (fmap IssueT.fuBranch1 fuActivation) gprPort1
        branchUnit2 = Violet.Backend.Branch.branch (fmap IssueT.fuBranch2 fuActivation) gprPort2
        (dcacheUnit1, dcWeReq, fastBusOut) = Violet.Backend.DCache.dcache dcacheImpl (fmap IssueT.fuMem1 fuActivation) gprPort1 dcWeCommit fastBusIn
        (ctrlUnit, ctrlBusy, sysOut, fastBusIn, icRefillIn) = unbundle $ Violet.Backend.Ctrl.ctrl (fmap IssueT.fuCtrl fuActivation) gprPort1 earlyExc sysIn perfCounters fastBusOut

        -- Late ALUs.
        lateBypassInput = register Violet.Backend.Issue.emptyBypassInput $ register Violet.Backend.Issue.emptyBypassInput bypassInput
        lateGprFetch = register undefined $ register undefined gprFetch

        lateAppend1A = commitPipe1 !! 2
        lateAppend1B = register PipeT.Bubble lateAppend1A
        lateAppend1C = register PipeT.Bubble lateAppend1B

        lateAppend2A = commitPipe2 !! 2
        lateAppend2B = register PipeT.Bubble lateAppend2A
        lateAppend2C = register PipeT.Bubble lateAppend2B

        lateCommitPipe1 = lateAppend1A :> lateAppend1B :> lateAppend1C :> Nil
        lateCommitPipe2 = lateAppend2A :> lateAppend2B :> lateAppend2C :> Nil

        (lateFuActivation, lateGprPort1, lateGprPort2) = Violet.Backend.Bypass.bypass lateBypassInput lateGprFetch lateCommitPipe1 lateCommitPipe2
        lateIntAlu1 = Violet.Backend.IntAlu.intAlu (fmap IssueT.fuLateInt1 lateFuActivation) lateGprPort1
        lateIntAlu2 = Violet.Backend.IntAlu.intAlu (fmap IssueT.fuLateInt2 lateFuActivation) lateGprPort2
        lateBranchUnit1 = Violet.Backend.Branch.branch (fmap IssueT.fuLateBranch1 lateFuActivation) lateGprPort1
        lateBranchUnit2 = Violet.Backend.Branch.branch (fmap IssueT.fuLateBranch2 lateFuActivation) lateGprPort2

        (gprWritePort1, gprWritePort2, backendCmd, dcWeCommit, commitLog, earlyExc, historyUpd, instRetire, branchStat) = unbundle $ Violet.Backend.Commit.commit $ bundle (last commitPipe1, last commitPipe2, last recoveryPipe, dcWeReq)

        perfCounters = Violet.Backend.PerfCounter.perfCounter instRetire branchStat

        -- XXX: `ctrlUnit` may send non-pipelined signals and therefore must has the highest priority.
        -- This should be fixed. 
        commitStagesIn1 =
            selectCommit (selectCommit intAlu1 branchUnit1) ctrlUnit
            :> commitPipe1 !! 0
            :> selectCommit (selectCommit (if Config.lateALU then selectCommit lateIntAlu1 lateBranchUnit1 else pure PipeT.Bubble) dcacheUnit1) (commitPipe1 !! 1)
            :> Nil
        commitStagesIn2 =
            selectCommit intAlu2 branchUnit2
            :> commitPipe2 !! 0
            :> selectCommit (if Config.lateALU then selectCommit lateIntAlu2 lateBranchUnit2 else pure PipeT.Bubble) (commitPipe2 !! 1)
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