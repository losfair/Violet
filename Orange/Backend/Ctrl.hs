module Orange.Backend.Ctrl where

import Clash.Prelude

import Orange.Types.Ctrl
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Issue as IssueT
import qualified Orange.Types.Pipe as PipeT
import qualified Orange.Types.Fetch as FetchT

type HighMulState = BitVector 2
data CtrlState = SIdle | SMul FetchT.PC GprT.RegIndex GprT.RegValue GprT.RegValue | SMulH FetchT.PC GprT.RegIndex HighMulState
    deriving (Generic, NFDataX, Eq, Show)
type MultiplierInput = (BitVector 64, BitVector 64)
type MultiplierOutput = BitVector 64

undefinedMultiplierInput :: MultiplierInput
undefinedMultiplierInput = (undefined, undefined)

ctrl' :: (CtrlState, CtrlBusy, MultiplierInput)
      -> (Maybe (IssueT.IssuePort, IssueT.ControlIssue), (GprT.RegValue, GprT.RegValue), MultiplierOutput)
      -> ((CtrlState, CtrlBusy, MultiplierInput), (PipeT.Commit, CtrlBusy, MultiplierInput))
ctrl' (state, busy, mulInput) (issue, (rs1V, rs2V), mulOut) = ((state', busy', mulInput'), (commit', busy, mulInput))
    where
        (state', commit', busy', mulInput') = case state of
            SIdle -> case issue of
                Just x -> onIssue x (rs1V, rs2V)
                Nothing -> (state, PipeT.Bubble, Idle, undefinedMultiplierInput)
            SMul pc rd rs1V rs2V -> (SIdle, PipeT.Ok (pc, Just $ PipeT.GPR rd (rs1V * rs2V)), Idle, undefinedMultiplierInput)
            SMulH pc rd s -> case s of
                0b00 -> (SMulH pc rd 0b01, PipeT.Bubble, Busy, undefinedMultiplierInput)
                0b01 -> (SMulH pc rd 0b10, PipeT.Bubble, Busy, undefinedMultiplierInput)
                0b10 -> (SMulH pc rd 0b11, PipeT.Bubble, Idle, undefinedMultiplierInput)
                0b11 -> (SIdle, PipeT.Ok (pc, Just $ PipeT.GPR rd (slice d63 d32 mulOut)), Idle, undefinedMultiplierInput)

onIssue :: (IssueT.IssuePort, IssueT.ControlIssue)
        -> (GprT.RegValue, GprT.RegValue)
        -> (CtrlState, PipeT.Commit, CtrlBusy, MultiplierInput)
onIssue ((pc, inst, md), IssueT.CtrlNormal) (rs1V, rs2V) = case slice d6 d0 inst of
    0b0110011 | testBit inst 25 -> -- mul/div
        case slice d14 d14 inst of
            0b0 -> -- mul*
                case slice d13 d12 inst of
                    0b00 ->
                        -- We don't need to activate the BUSY line here since MUL takes one cycle only
                        (SMul pc (GprT.decodeRd inst) rs1V rs2V, PipeT.Bubble, Idle, undefinedMultiplierInput)
                    x ->
                        let (src1, src2) = case x of
                                            0b01 -> -- mulh
                                                (signExtend rs1V, signExtend rs2V)
                                            0b10 -> -- mulhsu
                                                (signExtend rs1V, zeroExtend rs2V)
                                            _ -> -- 0b11: mulhu
                                                (zeroExtend rs1V, zeroExtend rs2V)
                            in
                                (SMulH pc (GprT.decodeRd inst) 0, PipeT.Bubble, Busy, (src1, src2))
            _ -> (SIdle, PipeT.Ok (pc, Nothing), Idle, undefinedMultiplierInput)
    _ -> (SIdle, PipeT.Ok (pc, Nothing), Idle, undefinedMultiplierInput)
onIssue ((pc, inst, md), IssueT.CtrlDecodeException) _ = (SIdle, PipeT.Ok (pc, Nothing), Idle, undefinedMultiplierInput)

ctrl :: HiddenClockResetEnable dom
     => Signal dom (Maybe (IssueT.IssuePort, IssueT.ControlIssue))
     -> Signal dom (GprT.RegValue, GprT.RegValue)
     -> Signal dom (PipeT.Commit, CtrlBusy)
ctrl issue gprPair = bundle $ (commit, busy)
    where
        m = mealy ctrl' (SIdle, Idle, undefinedMultiplierInput)
        (commit, busy, mulInput) = unbundle $ m $ bundle (issue, gprPair, mulOutput)
        mulOutput = multiplier mulInput

multiplier :: HiddenClockResetEnable dom
           => Signal dom MultiplierInput
           -> Signal dom MultiplierOutput
multiplier x = register 0 $ register 0 $ register 0 $ fmap (\(a, b) -> a * b) x
