module Violet.Backend.Commit where

import Clash.Prelude
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Pipe as PipeT
import qualified Violet.Types.DCache as DCacheT
import qualified Violet.Types.Commit as CommitT
import qualified Violet.Types.PerfCounter as PerfCounterT

data CommitState = NormalOperation | EarlyExceptionPending PipeT.EarlyException | ExceptionPending
    deriving (Generic, NFDataX, Eq)
data PrevExceptionState = HadException | HadEarlyException PipeT.EarlyException | NoException
    deriving (Generic, NFDataX, Eq)
data Resolution = ExcResolved | ExcNotResolved
    deriving (Generic, NFDataX, Eq)

commit' :: CommitState
        -> (PipeT.Commit, PipeT.Commit, PipeT.Recovery, DCacheT.WriteEnable)
        -> (
                CommitState,
                (
                    GprT.WritePort, GprT.WritePort, FetchT.BackendCmd, DCacheT.WriteEnable, CommitT.CommitLog,
                    Maybe PipeT.EarlyException, Maybe FetchT.HistoryUpdate, PerfCounterT.InstRetire, PerfCounterT.BranchStat
                )
            )
commit' s (cp1, cp2, rp, dcWe) = (s', out)
    where
        (pc1, wp1, bcmd1, earlyExc1, resolution1, historyUpd1) = transformCommit cp1
        (pc2_, wp2_, bcmd2, earlyExc2, _, _) = transformCommit cp2

        hadException = case (s, rp, resolution1) of
            (ExceptionPending, PipeT.IsRecovery, _) -> NoException
            (NormalOperation, _, _) -> NoException
            (EarlyExceptionPending _, _, ExcResolved) -> NoException
            (EarlyExceptionPending e, _, ExcNotResolved) -> HadEarlyException e
            _ -> HadException

        -- Don't commit port 2 if port 1 has exception
        -- `ExcResolved` implies `FetchT.ApplyBranch` so we don't need to explicitly select on it here
        wp2 = case (bcmd1, earlyExc1) of
            (FetchT.NoCmd, Nothing) -> wp2_
            _ -> Nothing
        pc2 = case (bcmd1, earlyExc1) of
            (FetchT.NoCmd, Nothing) -> pc2_
            _ -> Nothing

        -- Select the "earlier" backend command
        bcmd = case (bcmd1, earlyExc1) of
            (FetchT.NoCmd, Nothing) -> bcmd2
            _ -> bcmd1

        s' = case (hadException, bcmd, earlyExc1, earlyExc2) of
            (NoException, FetchT.NoCmd, Nothing, Nothing) -> NormalOperation
            (NoException, FetchT.NoCmd, Just e, _) -> EarlyExceptionPending e
            (NoException, FetchT.NoCmd, _, Just e) -> EarlyExceptionPending e
            (HadEarlyException e, _, _, _) -> EarlyExceptionPending e
            _ -> ExceptionPending

        log = CommitT.CommitLog {
            CommitT.pc1 = pc1,
            CommitT.pc2 = pc2,
            CommitT.writePort1 = wp1,
            CommitT.writePort2 = wp2
        }

        instRetire = PerfCounterT.InstRetire $ case (pc1, pc2) of
            (Nothing, Nothing) -> 0
            (Just _, Nothing) -> 1
            (Nothing, Just _) -> 1
            (Just _, Just _) -> 2

        branchStat = case (bcmd, historyUpd1) of
            (FetchT.ApplyBranch _, _) -> PerfCounterT.BranchMiss
            (_, Just _) -> PerfCounterT.BranchHit
            _ -> PerfCounterT.BranchNone

        -- DCache only commits on port 1 so we don't need to discard its result
        out = case hadException of
            HadException -> (Nothing, Nothing, FetchT.NoCmd, DCacheT.NoWrite, CommitT.emptyCommitLog, Nothing, Nothing, instRetire, PerfCounterT.BranchNone)
            HadEarlyException e -> (Nothing, Nothing, FetchT.NoCmd, DCacheT.NoWrite, CommitT.emptyCommitLog, Just e, Nothing, instRetire, PerfCounterT.BranchNone)
            NoException -> (wp1, wp2, bcmd, dcWe, log, Nothing, historyUpd1, instRetire, branchStat)

commit :: HiddenClockResetEnable dom
       => Signal dom (PipeT.Commit, PipeT.Commit, PipeT.Recovery, DCacheT.WriteEnable)
       -> Signal dom (GprT.WritePort, GprT.WritePort, FetchT.BackendCmd, DCacheT.WriteEnable, CommitT.CommitLog, Maybe PipeT.EarlyException, Maybe FetchT.HistoryUpdate, PerfCounterT.InstRetire, PerfCounterT.BranchStat)
commit = mealy commit' NormalOperation

transformCommit :: PipeT.Commit -> (Maybe FetchT.PC, GprT.WritePort, FetchT.BackendCmd, Maybe PipeT.EarlyException, Resolution, Maybe FetchT.HistoryUpdate)
transformCommit (PipeT.Ok (pc, Just (PipeT.GPR i v), historyUpd)) = (Just pc, Just (i, v), FetchT.NoCmd, Nothing, ExcNotResolved, historyUpd)
transformCommit (PipeT.Ok (pc, Nothing, historyUpd)) = (Just pc, Nothing, FetchT.NoCmd, Nothing, ExcNotResolved, historyUpd)
transformCommit PipeT.Bubble = (Nothing, Nothing, FetchT.NoCmd, Nothing, ExcNotResolved, Nothing)
transformCommit (PipeT.Exc (pc, e)) = case e of
    PipeT.EarlyExcResolution (nextPC, Just (PipeT.GPR i v)) -> (Just pc, Just (i, v), FetchT.ApplyBranch (nextPC, (pc, FetchT.NoPref)), Nothing, ExcResolved, Nothing)
    PipeT.EarlyExcResolution (nextPC, Nothing) -> (Just pc, Nothing, FetchT.ApplyBranch (nextPC, (pc, FetchT.NoPref)), Nothing, ExcResolved, Nothing)
    PipeT.BranchLink nextPC idx linkPC -> (Just pc, Just (idx, linkPC), FetchT.ApplyBranch (nextPC, (pc, FetchT.NoPref)), Nothing, ExcNotResolved, Nothing)
    PipeT.BranchFalsePos nextPC history -> (Just pc, Nothing, FetchT.ApplyBranch (nextPC, (pc, FetchT.NotTaken history)), Nothing, ExcNotResolved, Nothing)
    PipeT.BranchFalseNeg nextPC history -> (Just pc, Nothing, FetchT.ApplyBranch (nextPC, (pc, FetchT.Taken history)), Nothing, ExcNotResolved, Nothing)
    PipeT.EarlyExc e -> (Nothing, Nothing, FetchT.NoCmd, Just e, ExcNotResolved, Nothing)
