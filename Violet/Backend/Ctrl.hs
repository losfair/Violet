module Violet.Backend.Ctrl where

import Clash.Prelude

import Violet.Types.Ctrl
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Issue as IssueT
import qualified Violet.Types.Pipe as PipeT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.DCache as DCacheT

type HighMulState = BitVector 2
data CtrlState = SIdle
    | SWaitForEarlyExcAck
    | SMul FetchT.PC GprT.RegIndex GprT.RegValue GprT.RegValue
    | SMulH FetchT.PC GprT.RegIndex HighMulState
    | SDiv FetchT.PC GprT.RegIndex DivType DivState
    | SDivEnd FetchT.PC GprT.RegIndex DivType DivState
    | SCsrReadWrite FetchT.PC GprT.RegIndex CsrIndex GprT.RegValue
    | SIOMemRead FetchT.PC GprT.RegIndex DCacheT.MemAddr
    | SIOMemWrite FetchT.PC DCacheT.MemAddr DCacheT.MemData
    deriving (Generic, NFDataX, Eq, Show)
type MultiplierInput = (BitVector 64, BitVector 64)
type MultiplierOutput = BitVector 64
data DivType = SignedDiv Bool Bool | UnsignedDiv | SignedRem Bool Bool | UnsignedRem
    deriving (Generic, NFDataX, Eq, Show)
data DivState = DivState {
    divDividend :: BitVector 32,
    divDivisor :: BitVector 32,
    divQuotient :: BitVector 32,
    divRemainder :: BitVector 32,
    divCounter :: Index 32
} deriving (Generic, NFDataX, Eq, Show)

undefinedMultiplierInput :: MultiplierInput
undefinedMultiplierInput = (undefined, undefined)

ctrl' :: (CtrlState, CtrlBusy, MultiplierInput)
      -> (Maybe (IssueT.IssuePort, IssueT.ControlIssue), (GprT.RegValue, GprT.RegValue), Maybe PipeT.EarlyException, MultiplierOutput, SystemBusIn)
      -> ((CtrlState, CtrlBusy, MultiplierInput), (PipeT.Commit, CtrlBusy, MultiplierInput, SystemBusOut))
ctrl' (state, busy, mulInput) (issue, (rs1V, rs2V), earlyExc, mulOut, sysIn) = ((state', busy', mulInput'), (commit', busy, mulInput, bus))
    where
        (state', commit', busy', mulInput') = case state of
            SIdle -> case (earlyExc, issue) of
                (Just earlyExc, _) -> onEarlyExc earlyExc
                (_, Just x) -> onIssue x (rs1V, rs2V)
                (_, Nothing) -> (state, PipeT.Bubble, Idle, undefinedMultiplierInput)
            SWaitForEarlyExcAck -> case earlyExc of
                Just _ -> (SWaitForEarlyExcAck, PipeT.Bubble, Idle, undefinedMultiplierInput)
                Nothing -> (SIdle, PipeT.Bubble, Idle, undefinedMultiplierInput)
            SMul pc rd rs1V rs2V -> (SIdle, PipeT.Ok (pc, Just $ PipeT.GPR rd (rs1V * rs2V)), Idle, undefinedMultiplierInput)
            SMulH pc rd s -> case s of
                0b00 -> (SMulH pc rd 0b01, PipeT.Bubble, Busy, undefinedMultiplierInput)
                0b01 -> (SMulH pc rd 0b10, PipeT.Bubble, Busy, undefinedMultiplierInput)
                0b10 -> (SMulH pc rd 0b11, PipeT.Bubble, Idle, undefinedMultiplierInput)
                0b11 -> (SIdle, PipeT.Ok (pc, Just $ PipeT.GPR rd (slice d63 d32 mulOut)), Idle, undefinedMultiplierInput)
            SDiv pc rd ty ds -> (nextS, PipeT.Bubble, Busy, undefinedMultiplierInput)
                where
                    remainder_ = slice d30 d0 (divRemainder ds) ++# slice d31 d31 (divDividend ds)
                    dividend' = slice d30 d0 (divDividend ds) ++# 0
                    (remainder', quotient') =
                        if remainder_ >= (divDivisor ds) then
                            (remainder_ - divDivisor ds, slice d30 d0 (divQuotient ds) ++# 1)
                        else
                            (remainder_, slice d30 d0 (divQuotient ds) ++# 0)
                    ds' = DivState { divDividend = dividend', divDivisor = divDivisor ds, divQuotient = quotient', divRemainder = remainder', divCounter = divCounter ds + 1 }
                    nextS = if divCounter ds == maxBound then SDivEnd pc rd ty ds' else SDiv pc rd ty ds'
            SDivEnd pc rd ty ds -> (SIdle, PipeT.Ok (pc, Just (PipeT.GPR rd v)), Idle, undefinedMultiplierInput)
                where
                    v = case ty of
                        SignedDiv aNeg bNeg -> if xor aNeg bNeg then -(divQuotient ds) else divQuotient ds
                        UnsignedDiv -> divQuotient ds
                        SignedRem aNeg bNeg -> if aNeg then -(divRemainder ds) else divRemainder ds
                        UnsignedRem -> divRemainder ds
            SCsrReadWrite pc dst i v -> (SWaitForEarlyExcAck, PipeT.Exc (pc, PipeT.EarlyExcResolution (pc + 4, Nothing)), Idle, undefinedMultiplierInput)
            SIOMemRead pc dst _ -> case iIoReady (iIoBus sysIn) of
                True -> (SWaitForEarlyExcAck, PipeT.Exc (pc, PipeT.EarlyExcResolution (pc + 4, Just (PipeT.GPR dst $ iIoData (iIoBus sysIn)))), Idle, undefinedMultiplierInput)
                False -> (state, PipeT.Bubble, Busy, undefinedMultiplierInput)
            SIOMemWrite pc _ _ -> case iIoReady (iIoBus sysIn) of
                True -> (SWaitForEarlyExcAck, PipeT.Exc (pc, PipeT.EarlyExcResolution (pc + 4, Nothing)), Idle, undefinedMultiplierInput)
                False -> (state, PipeT.Bubble, Busy, undefinedMultiplierInput)
        bus = case state of
            SIOMemRead pc dst addr -> idleSystemBusOut { oIoBus = IOBusOut { oIoValid = True, oIoWrite = False, oIoAddr = addr, oIoData = undefined } }
            SIOMemWrite pc addr d -> idleSystemBusOut { oIoBus = IOBusOut { oIoValid = True, oIoWrite = True, oIoAddr = addr, oIoData = d } }
            _ -> idleSystemBusOut

onEarlyExc :: PipeT.EarlyException -> (CtrlState, PipeT.Commit, CtrlBusy, MultiplierInput)
onEarlyExc e = case e of
    PipeT.CsrReadWrite pc dst i v -> (SCsrReadWrite pc dst i v, PipeT.Bubble, Busy, undefinedMultiplierInput)
    PipeT.DecodeFailure pc -> (SWaitForEarlyExcAck, PipeT.Exc (pc, PipeT.EarlyExcResolution (pc + 4, Nothing)), Idle, undefinedMultiplierInput)
    PipeT.IOMemRead pc dst addr -> (SIOMemRead pc dst addr, PipeT.Bubble, Busy, undefinedMultiplierInput)
    PipeT.IOMemWrite pc addr val -> (SIOMemWrite pc addr val, PipeT.Bubble, Busy, undefinedMultiplierInput)

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
            0b1 -> -- div(u)/rem(u)
                case slice d13 d12 inst of
                    0b00 -> -- div
                        (SDiv pc (GprT.decodeRd inst) (SignedDiv (testBit rs1V 31) (testBit rs2V 31)) (mkDivStateS rs1V rs2V), PipeT.Bubble, Busy, undefinedMultiplierInput)
                    0b01 -> -- divu
                        (SDiv pc (GprT.decodeRd inst) UnsignedDiv (mkDivStateU rs1V rs2V), PipeT.Bubble, Busy, undefinedMultiplierInput)
                    0b10 -> -- rem
                        (SDiv pc (GprT.decodeRd inst) (SignedRem (testBit rs1V 31) (testBit rs2V 31)) (mkDivStateS rs1V rs2V), PipeT.Bubble, Busy, undefinedMultiplierInput)
                    0b11 -> -- remu
                        (SDiv pc (GprT.decodeRd inst) UnsignedRem (mkDivStateU rs1V rs2V), PipeT.Bubble, Busy, undefinedMultiplierInput)
    _ -> (SIdle, PipeT.Ok (pc, Nothing), Idle, undefinedMultiplierInput)
onIssue ((pc, inst, md), IssueT.CtrlDecodeException) _ = (SIdle, PipeT.Exc (pc, PipeT.EarlyExc $ PipeT.DecodeFailure pc), Idle, undefinedMultiplierInput)

ctrl :: HiddenClockResetEnable dom
     => Signal dom (Maybe (IssueT.IssuePort, IssueT.ControlIssue))
     -> Signal dom (GprT.RegValue, GprT.RegValue)
     -> Signal dom (Maybe PipeT.EarlyException)
     -> Signal dom SystemBusIn
     -> Signal dom (PipeT.Commit, CtrlBusy, SystemBusOut)
ctrl issue gprPair earlyExc sysIn = bundle $ (commit, busy, sysOut)
    where
        m = mealy ctrl' (SIdle, Idle, undefinedMultiplierInput)
        (commit, busy, mulInput, sysOut) = unbundle $ m $ bundle (issue, gprPair, earlyExc, mulOutput, sysIn)
        mulOutput = multiplier mulInput

multiplier :: HiddenClockResetEnable dom
           => Signal dom MultiplierInput
           -> Signal dom MultiplierOutput
multiplier x = register 0 $ register 0 $ register 0 $ fmap (\(a, b) -> a * b) x

mkDivStateU :: BitVector 32 -> BitVector 32 -> DivState
mkDivStateU a b = DivState { divDividend = a, divDivisor = b, divQuotient = 0, divRemainder = 0, divCounter = 0 }

mkDivStateS :: BitVector 32 -> BitVector 32 -> DivState
mkDivStateS a b = DivState { divDividend = if testBit a 31 then -a else a, divDivisor = if testBit b 31 then -b else b, divQuotient = 0, divRemainder = 0, divCounter = 0 }
