module Orange.Backend.Bypass where

import Clash.Prelude
import Orange.Types.Issue
import qualified Orange.Types.Pipe as PipeT
import qualified Orange.Types.Issue as IssueT
import qualified Orange.Types.Gpr as GprT

bypassOne' :: (
                Vec PipeT.PipeSize PipeT.Commit,
                Vec PipeT.PipeSize PipeT.Commit,
                (GprT.RegValue, GprT.RegValue),
                IssueT.IssuePort)
           -> (GprT.RegValue, GprT.RegValue)
bypassOne' (cp1, cp2, (rs1Fetched, rs2Fetched), (_, inst, _)) = (overrideRegfetch rs1Fetched rs1Bypassed, overrideRegfetch rs2Fetched rs2Bypassed)
    where
        (rs1, rs2) = GprT.decodeRs inst
        rs1Bypassed = foldCommitPipes rs1 cp1 cp2
        rs2Bypassed = foldCommitPipes rs2 cp1 cp2

bypass :: HiddenClockResetEnable dom
       => Signal dom ((IssuePort, IssuePort), ActivationMask)
       -> Signal dom ((GprT.RegValue, GprT.RegValue), (GprT.RegValue, GprT.RegValue))
       -> Vec PipeT.PipeSize (Signal dom PipeT.Commit)
       -> Vec PipeT.PipeSize (Signal dom PipeT.Commit)
       -> (Signal dom FunctionUnitActivation, Signal dom (GprT.RegValue, GprT.RegValue), Signal dom (GprT.RegValue, GprT.RegValue))
bypass issueInfo gprFetch cp1_ cp2_ = (activation, bypass1, bypass2)
    where
        (issuePorts, actMask) = unbundle issueInfo
        (issue1, issue2) = unbundle issuePorts
        (rf1, rf2) = unbundle gprFetch
        cp1 = bundle cp1_
        cp2 = bundle cp2_
        bypass1 = fmap bypassOne' $ bundle (cp1, cp2, rf1, issue1)
        bypass2 = fmap bypassOne' $ bundle (cp1, cp2, rf2, issue2)
        activation = fmap (\(a, b, c) -> maskActivation a b c) $ bundle (actMask, issue1, issue2)

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

maskActivation :: ActivationMask -> IssuePort -> IssuePort -> FunctionUnitActivation
maskActivation mask port1 port2 = FunctionUnitActivation {
    fuInt1 = if amInt1 mask then Just port1 else Nothing,
    fuInt2 = if amInt2 mask then Just port2 else Nothing,
    fuBranch = if amBranch mask then Just port1 else Nothing,
    fuMem = if amMem mask then Just port1 else Nothing,
    fuCtrl = case amCtrl mask of
        Just ctrlIssue -> Just (port1, ctrlIssue)
        Nothing -> Nothing
}
