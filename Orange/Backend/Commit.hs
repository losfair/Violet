module Orange.Backend.Commit where

import Clash.Prelude
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Pipe as PipeT
import qualified Orange.Types.DCache as DCacheT
import qualified Orange.Types.Commit as CommitT

data CommitState = NormalOperation | ExceptionPending
    deriving (Generic, NFDataX, Eq)
data PrevExceptionState = HadException | NoException
    deriving (Generic, NFDataX, Eq)

commit' :: CommitState
        -> (PipeT.Commit, PipeT.Commit, PipeT.Recovery, DCacheT.WriteEnable)
        -> (CommitState, (GprT.WritePort, GprT.WritePort, FetchT.BackendCmd, DCacheT.WriteEnable, CommitT.CommitLog))
commit' s (cp1, cp2, rp, dcWe) = (s', out)
    where
        hadException = case (s, rp) of
            (ExceptionPending, PipeT.IsRecovery) -> NoException
            (NormalOperation, _) -> NoException
            _ -> HadException

        (pc1, wp1, bcmd1) = transformCommit cp1
        (pc2_, wp2_, bcmd2) = transformCommit cp2

        -- Don't commit port 2 if port 1 has exception
        wp2 = case bcmd1 of
            FetchT.NoCmd -> wp2_
            _ -> Nothing
        pc2 = case bcmd1 of
            FetchT.NoCmd -> pc2_
            _ -> Nothing

        -- Select the "earlier" backend command
        bcmd = case bcmd1 of
            FetchT.ApplyBranch _ -> bcmd1
            _ -> bcmd2

        s' = case (hadException, bcmd) of
            (NoException, FetchT.NoCmd) -> NormalOperation
            _ -> ExceptionPending

        log = CommitT.CommitLog {
            CommitT.pc1 = pc1,
            CommitT.pc2 = pc2,
            CommitT.writePort1 = wp1,
            CommitT.writePort2 = wp2
        }

        -- DCache only commits on port 1 so we don't need to discard its result
        out = case hadException of
            HadException -> (Nothing, Nothing, FetchT.NoCmd, DCacheT.NoWrite, CommitT.emptyCommitLog)
            NoException -> (wp1, wp2, bcmd, dcWe, log)

commit :: HiddenClockResetEnable dom
       => Signal dom (PipeT.Commit, PipeT.Commit, PipeT.Recovery, DCacheT.WriteEnable)
       -> Signal dom (GprT.WritePort, GprT.WritePort, FetchT.BackendCmd, DCacheT.WriteEnable, CommitT.CommitLog)
commit = mealy commit' NormalOperation

transformCommit :: PipeT.Commit -> (Maybe FetchT.PC, GprT.WritePort, FetchT.BackendCmd)
transformCommit (PipeT.Ok (pc, Just (PipeT.GPR i v))) = (Just pc, Just (i, v), FetchT.NoCmd)
transformCommit (PipeT.Ok (pc, Nothing)) = (Just pc, Nothing, FetchT.NoCmd)
transformCommit PipeT.Bubble = (Nothing, Nothing, FetchT.NoCmd)
transformCommit (PipeT.Exc (pc, e)) = case e of
    PipeT.BranchLink nextPC idx linkPC -> (Just pc, Just (idx, linkPC), FetchT.ApplyBranch (nextPC, (pc, FetchT.Unconditional)))
    PipeT.BranchFalsePos nextPC -> (Just pc, Nothing, FetchT.ApplyBranch (nextPC, (pc, FetchT.NotTaken)))
    PipeT.BranchFalseNeg nextPC -> (Just pc, Nothing, FetchT.ApplyBranch (nextPC, (pc, FetchT.Taken)))
    _ -> (Just pc, Nothing, FetchT.NoCmd) -- TODO: Implement exceptions
