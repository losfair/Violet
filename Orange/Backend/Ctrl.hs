module Orange.Backend.Ctrl where

import Clash.Prelude

import Orange.Types.Ctrl
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Issue as IssueT
import qualified Orange.Types.Pipe as PipeT
import qualified Orange.Types.Fetch as FetchT

type HighMulState = BitVector 2
data CtrlState = SIdle | SMul FetchT.PC GprT.RegIndex GprT.RegValue GprT.RegValue | SMulH FetchT.PC GprT.RegIndex (BitVector 64) (BitVector 64) HighMulState
    deriving (Generic, NFDataX, Eq, Show)

ctrl' :: (CtrlState, CtrlBusy)
      -> (Maybe (IssueT.IssuePort, IssueT.ControlIssue), (GprT.RegValue, GprT.RegValue))
      -> ((CtrlState, CtrlBusy), (PipeT.Commit, CtrlBusy))
ctrl' (state, busy) (issue, (rs1V, rs2V)) = ((state', busy'), (commit', busy))
    where
        (state', commit', busy') = case state of
            SIdle -> case issue of
                Just x -> onIssue x (rs1V, rs2V)
                Nothing -> (state, PipeT.Bubble, Idle)
            SMul pc rd rs1V rs2V -> (SIdle, PipeT.Ok (pc, Just $ PipeT.GPR rd (rs1V * rs2V)), Idle)

onIssue :: (IssueT.IssuePort, IssueT.ControlIssue)
        -> (GprT.RegValue, GprT.RegValue)
        -> (CtrlState, PipeT.Commit, CtrlBusy)
onIssue ((pc, inst, md), IssueT.CtrlNormal) (rs1V, rs2V) = case slice d6 d0 inst of
    0b0110011 | testBit inst 25 -> -- mul/div
        case slice d14 d14 inst of
            0b0 -> -- mul*
                case slice d13 d12 inst of
                    0b00 ->
                        -- We don't need to activate the BUSY line here since MUL takes one cycle only
                        (SMul pc (GprT.decodeRd inst) rs1V rs2V, PipeT.Bubble, Idle)
                    x -> 
                        (SIdle, PipeT.Ok (pc, Nothing), Idle)
                        -- FIXME: Bad timing with mulh*
                        {-|
                        let (src1, src2) = case x of
                                            0b01 -> -- mulh
                                                (signExtend rs1V, signExtend rs2V)
                                            0b10 -> -- mulhsu
                                                (signExtend rs1V, zeroExtend rs2V)
                                            _ -> -- 0b11: mulhu
                                                (zeroExtend rs1V, zeroExtend rs2V)
                            in
                                (SMulH pc (GprT.decodeRd inst) src1 src2 0, PipeT.Bubble, Busy)
                        |-}
            _ -> (SIdle, PipeT.Ok (pc, Nothing), Idle)
    _ -> (SIdle, PipeT.Ok (pc, Nothing), Idle)
onIssue ((pc, inst, md), IssueT.CtrlDecodeException) _ = (SIdle, PipeT.Ok (pc, Nothing), Idle)

ctrl :: HiddenClockResetEnable dom
     => Signal dom (Maybe (IssueT.IssuePort, IssueT.ControlIssue))
     -> Signal dom (GprT.RegValue, GprT.RegValue)
     -> Signal dom (PipeT.Commit, CtrlBusy)
ctrl issue gprPair = m $ bundle (issue, gprPair)
    where
        m = mealy ctrl' (SIdle, Idle)
