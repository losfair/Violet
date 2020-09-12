module Orange.Backend.Commit where

import Clash.Prelude
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Pipe as PipeT
import qualified Orange.Types.DCache as DCacheT

data CommitState = NormalOperation | ExceptionPending
    deriving (Generic, NFDataX, Eq)
data PrevExceptionState = HadException | NoException
    deriving (Generic, NFDataX, Eq)

commit' :: CommitState
        -> (PipeT.Commit, PipeT.Commit, PipeT.Recovery, DCacheT.WriteEnable)
        -> (CommitState, (GprT.WritePort, GprT.WritePort, FetchT.BackendCmd, DCacheT.WriteEnable))
commit' s (cp1, cp2, rp, dcWe) = (s', out)
    where
        hadException = case (s, rp) of
            (ExceptionPending, PipeT.IsRecovery) -> NoException
            (NormalOperation, _) -> NoException
            _ -> HadException

        (wp1, bcmd1) = transformCommit cp1
        (wp2_, bcmd2) = transformCommit cp2

        -- Don't commit port 2 if port 1 has exception
        wp2 = case bcmd1 of
            FetchT.NoCmd -> wp2_
            _ -> Nothing

        -- Select the "earlier" backend command
        bcmd = case bcmd1 of
            FetchT.ApplyBranch _ -> bcmd1
            _ -> bcmd2

        s' = case (hadException, bcmd) of
            (NoException, FetchT.NoCmd) -> NormalOperation
            _ -> ExceptionPending

        -- DCache only commits on port 1 so we don't need to discard its result
        out = case hadException of
            HadException -> (Nothing, Nothing, FetchT.NoCmd, DCacheT.NoWrite)
            NoException -> (wp1, wp2, bcmd, dcWe)

commit :: HiddenClockResetEnable dom
       => Signal dom (PipeT.Commit, PipeT.Commit, PipeT.Recovery, DCacheT.WriteEnable)
       -> Signal dom (GprT.WritePort, GprT.WritePort, FetchT.BackendCmd, DCacheT.WriteEnable)
commit = mealy commit' NormalOperation

transformCommit :: PipeT.Commit -> (GprT.WritePort, FetchT.BackendCmd)
transformCommit (PipeT.Ok (pc, Just (PipeT.GPR i v))) = (Just (i, v), FetchT.NoCmd)
transformCommit (PipeT.Ok (pc, Nothing)) = (Nothing, FetchT.NoCmd)
transformCommit PipeT.Bubble = (Nothing, FetchT.NoCmd)
transformCommit (PipeT.Exc (pc, e)) = case e of
    PipeT.BranchLink nextPC idx -> (Just (idx, pc + 4), FetchT.ApplyBranch (nextPC, Nothing))
    PipeT.BranchFalsePos -> (Nothing, FetchT.ApplyBranch (pc + 4, Just (pc, FetchT.NotTaken)))
    PipeT.BranchFalseNeg nextPC -> (Nothing, FetchT.ApplyBranch (nextPC, Just (pc, FetchT.Taken)))
    _ -> (Nothing, FetchT.NoCmd) -- TODO: Implement exceptions
