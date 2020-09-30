module Violet.Backend.Branch where

import Clash.Prelude
import qualified Violet.Types.Pipe as PipeT
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Issue as IssueT
import qualified Violet.Types.Fetch as FetchT

branch' :: Maybe IssueT.IssuePort
        -> (GprT.RegValue, GprT.RegValue)
        -> PipeT.Commit
branch' Nothing _ = PipeT.Bubble
branch' (Just (pc, inst, meta)) (rs1V, rs2V) = commit
    where
        mode = slice d3 d2 inst
        rd = GprT.decodeRd inst
        condSatisfied_ = case slice d14 d13 inst of
            0b00 -> rs1V == rs2V
            0b10 -> signedLt rs1V rs2V
            _ -> unsignedLt rs1V rs2V -- 0b11
        condSatisfied = case slice d12 d12 inst of
            0b0 -> condSatisfied_
            0b1 -> not condSatisfied_
        commit = case mode of
            0b11 -> -- jal
                if FetchT.branchPredicted meta == Just (pc + FetchT.decodeJalOffset inst) then
                    PipeT.Ok (pc, Just (PipeT.GPR rd (pc + 4)), Nothing)
                else
                    PipeT.Exc (pc, PipeT.BranchLink (pc + FetchT.decodeJalOffset inst) rd (pc + 4))
            0b01 -> -- jalr: link
                let dst = clearBit (rs1V + signExtend (slice d31 d20 inst)) 0 in
                    if FetchT.branchPredicted meta == Just dst then
                        PipeT.Ok (pc, Just (PipeT.GPR rd (pc + 4)), Nothing)
                    else
                        PipeT.Exc (pc, PipeT.BranchLink dst rd (pc + 4))
            _ -> -- 0b00: bcond
                if not condSatisfied && FetchT.branchPredicted meta == Nothing then
                    PipeT.Ok (pc, Nothing, Just FetchT.HistoryUpdate { FetchT.hFrom = pc, FetchT.hTaken = False, FetchT.hHistory = FetchT.globalHistory meta })
                else if condSatisfied && FetchT.branchPredicted meta == Just (pc + FetchT.decodeRelBrOffset inst) then
                    PipeT.Ok (pc, Nothing, Just FetchT.HistoryUpdate { FetchT.hFrom = pc, FetchT.hTaken = True, FetchT.hHistory = FetchT.globalHistory meta })
                else if not condSatisfied then
                    PipeT.Exc (pc, PipeT.BranchFalsePos (pc + 4) (FetchT.globalHistory meta))
                else -- condSatisfied
                    PipeT.Exc (pc, PipeT.BranchFalseNeg (pc + FetchT.decodeRelBrOffset inst) (FetchT.globalHistory meta))

branch :: HiddenClockResetEnable dom
       => Signal dom (Maybe IssueT.IssuePort)
       -> Signal dom (GprT.RegValue, GprT.RegValue)
       -> Signal dom PipeT.Commit
branch a b = fmap (\(a, b) -> branch' a b) $ bundle (a, b)

unsignedLt :: GprT.RegValue -> GprT.RegValue -> Bool
unsignedLt a b = (unpack a :: Unsigned 32) < (unpack b :: Unsigned 32)

signedLt :: GprT.RegValue -> GprT.RegValue -> Bool
signedLt a b = (unpack a :: Signed 32) < (unpack b :: Signed 32)