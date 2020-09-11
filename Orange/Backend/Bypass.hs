module Orange.Backend.Bypass where

import Clash.Prelude
import qualified Orange.Types.Pipe as PipeT
import qualified Orange.Types.Issue as IssueT
import qualified Orange.Types.Gpr as GprT

bypass' :: Vec PipeT.PipeSize PipeT.Commit
        -> Vec PipeT.PipeSize PipeT.Commit
        -> (GprT.RegValue, GprT.RegValue)
        -> IssueT.IssuePort
        -> (GprT.RegValue, GprT.RegValue)
bypass' cp1 cp2 (rs1Fetched, rs2Fetched) (_, inst, _) = (overrideRegfetch rs1Fetched rs1Bypassed, overrideRegfetch rs2Fetched rs2Bypassed)
    where
        (rs1, rs2) = GprT.decodeRs inst
        rs1Bypassed = foldCommitPipes rs1 cp1 cp2
        rs2Bypassed = foldCommitPipes rs2 cp1 cp2

foldCommitPipes :: GprT.RegIndex
                -> Vec PipeT.PipeSize PipeT.Commit
                -> Vec PipeT.PipeSize PipeT.Commit
                -> Maybe GprT.RegValue
foldCommitPipes i cp1 cp2 = fold folder mappedCp
    where
        combinedCp = merge cp1 cp2
        mappedCp = map mapper combinedCp

        mapper (PipeT.Ok (_, Just (PipeT.GPR i' v))) | i' == i = Just v
        mapper _ = Nothing

        folder (Just x) _ = Just x
        folder _ (Just x) = Just x
        folder _ _ = Nothing 

overrideRegfetch :: GprT.RegValue -> Maybe GprT.RegValue -> GprT.RegValue
overrideRegfetch fetched (Just x) = x
overrideRegfetch fetched _ = fetched
