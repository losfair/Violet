module Orange.Backend.Branch where

import Clash.Prelude
import qualified Orange.Types.Pipe as PipeT
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Issue as IssueT
import qualified Orange.Types.Fetch as FetchT

branch :: Maybe IssueT.IssuePort
        -> (GprT.RegValue, GprT.RegValue)
        -> PipeT.Commit
branch Nothing _ = PipeT.Bubble
branch (Just (pc, inst, meta)) (rs1V, rs2V) = commit
    where
        mode = slice d3 d2 inst
        rd = GprT.decodeRd inst
        relOffset = signExtend (slice d31 d31 inst ++# slice d7 d7 inst ++# slice d30 d25 inst ++# slice d11 d8 inst) :: BitVector 32
        condSatisfied_ = case slice d14 d13 inst of
            0b00 -> rs1V /= rs2V
            0b10 -> signedLt rs1V rs2V
            _ -> unsignedLt rs1V rs2V -- 0b11
        condSatisfied = case slice d12 d12 inst of
            0b0 -> condSatisfied_
            0b1 -> not condSatisfied_
        commit = case mode of
            0b11 -> -- jal: handled by frontend
                PipeT.Ok (pc, Just $ PipeT.GPR rd (pc + 4))
            0b01 -> -- jalr: link
                PipeT.Exc (pc, PipeT.BranchLink rs1V rd)
            _ -> -- 0b00: bcond
                if condSatisfied == FetchT.branchPredicted meta then
                    PipeT.Ok (pc, Nothing)
                else if condSatisfied then PipeT.Exc (pc, PipeT.BranchFalseNeg (pc + relOffset))
                else PipeT.Exc (pc, PipeT.BranchFalsePos)

unsignedLt :: GprT.RegValue -> GprT.RegValue -> Bool
unsignedLt a b = (unpack a :: Unsigned 32) < (unpack b :: Unsigned 32)

signedLt :: GprT.RegValue -> GprT.RegValue -> Bool
signedLt a b = (unpack a :: Signed 32) < (unpack b :: Signed 32)