module Violet.Backend.DCache where

import Clash.Prelude

import Violet.Types.DCache
import Violet.Types.Ctrl
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Issue as IssueT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Pipe as PipeT

maddr' :: Maybe IssueT.IssuePort
      -> (GprT.RegValue, GprT.RegValue)
      -> Maybe (FetchT.PC, MemAddr, Access)
maddr' Nothing _ = Nothing
maddr' (Just (pc, inst, _)) (rs1V, rs2V) = Just (pc, addr, access)
    where
        isStore = testBit inst 5
        offset_ = case isStore of
            True -> slice d31 d25 inst ++# slice d11 d7 inst
            False -> slice d31 d20 inst
        offset = signExtend offset_ :: BitVector 32
        addr = rs1V + offset
        rd = GprT.decodeRd inst
        selector = decodeSel (addr, inst)
        (writeData, writeMask) = prepareWrite selector rs2V
        signExt = if testBit inst 14 then UseZeroExtend else UseSignExtend
        access = case isStore of
            True -> WriteAccess (writeData, writeMask)
            False -> ReadAccess (rd, selector, signExt)

dcache :: HiddenClockResetEnable dom
       => DCacheImpl a
       => a
       -> Signal dom (Maybe IssueT.IssuePort)
       -> Signal dom (GprT.RegValue, GprT.RegValue)
       -> Signal dom WriteEnable
       -> Signal dom FastBusIn
       -> (Signal dom PipeT.Commit, Signal dom WriteEnable, Signal dom FastBusOut)
dcache impl issue regs weCommit fastBusIn = (commit, weReq, fastBusOut)
    where
        implIssue = register Nothing (fmap (\(x, y) -> maddr' x y) $ bundle (issue, regs))
        (commit, weReq, fastBusOut) = issueAccess impl implIssue weCommit fastBusIn

decodeSel :: (MemAddr, FetchT.Inst) -> Selector
decodeSel (addr, inst) = sel
    where
        width = slice d13 d12 inst -- 0b00: byte, 0b01: half, 0b10: word
        addrTail = slice d1 d0 addr
        sel = case (width, addrTail) of
            (0b00, 0b00) -> SelByte0
            (0b00, 0b01) -> SelByte1
            (0b00, 0b10) -> SelByte2
            (0b00, 0b11) -> SelByte3
            (0b01, 0b00) -> SelHalf0
            (0b01, _) -> SelHalf1 -- 0b10
            (_, _) -> SelWord

prepareWrite :: Selector -> MemData -> (MemData, WriteMask)
prepareWrite s i = case s of
    SelByte0 -> (i, 0b0001)
    SelByte1 -> (shiftL i 8, 0b0010)
    SelByte2 -> (shiftL i 16, 0b0100)
    SelByte3 -> (shiftL i 24, 0b1000)
    SelHalf0 -> (i, 0b0011)
    SelHalf1 -> (shiftL i 16, 0b1100)
    SelWord -> (i, 0b1111)
