module Violet.Backend.Gpr where

import Clash.Prelude
import Violet.Types.Gpr
import qualified Violet.Types.Fifo as FifoT

gpr :: HiddenClockResetEnable dom
    => Signal dom ((FifoT.FifoItem, FifoT.FifoItem), WritePort, WritePort)
    -> Signal dom ((RegValue, RegValue), (RegValue, RegValue))
gpr inputs = bundle (bundle (read1, read2), bundle (read3, read4))
    where
        (inPorts, wp1, wp2) = unbundle inputs
        (port1, port2) = unbundle inPorts
        (i1, i2) = unbundle $ fmap fifoItemToRegIndices port1
        (i3, i4) = unbundle $ fmap fifoItemToRegIndices port2

        readA1 = asyncRamPow2 i1 (fmap transformWp wp1)
        readA2 = asyncRamPow2 i2 (fmap transformWp wp1)
        readA3 = asyncRamPow2 i3 (fmap transformWp wp1)
        readA4 = asyncRamPow2 i4 (fmap transformWp wp1)
        readB1 = asyncRamPow2 i1 (fmap transformWp wp2)
        readB2 = asyncRamPow2 i2 (fmap transformWp wp2)
        readB3 = asyncRamPow2 i3 (fmap transformWp wp2)
        readB4 = asyncRamPow2 i4 (fmap transformWp wp2)

        (a1, a2, a3, a4) = unbundle $ (mealy mkAllocation 0) (bundle (wp1, wp2, i1, i2, i3, i4))

        read1 = register undefined $ fmap selectReadRes $ bundle (a1, i1, readA1, readB1, wp1, wp2)
        read2 = register undefined $ fmap selectReadRes $ bundle (a2, i2, readA2, readB2, wp1, wp2)
        read3 = register undefined $ fmap selectReadRes $ bundle (a3, i3, readA3, readB3, wp1, wp2)
        read4 = register undefined $ fmap selectReadRes $ bundle (a4, i4, readA4, readB4, wp1, wp2)

fifoItemToRegIndices :: FifoT.FifoItem -> (Unsigned 5, Unsigned 5)
fifoItemToRegIndices item = (rs1, rs2)
    where
        inst = case item of
            FifoT.Item (_, inst, _) -> inst
            _ -> undefined
        rs1 = unpack $ slice d19 d15 inst
        rs2 = unpack $ slice d24 d20 inst

transformWp :: WritePort -> Maybe (Unsigned 5, RegValue)
transformWp (Just (i, v)) = Just (unpack i, v)
transformWp _ = Nothing

mkAllocation :: BitVector 32
             -> (WritePort, WritePort, Unsigned 5, Unsigned 5, Unsigned 5, Unsigned 5)
             -> (BitVector 32, (Bool, Bool, Bool, Bool))
mkAllocation s (wp1, wp2, i1, i2, i3, i4) = (s', (a1, a2, a3, a4))
    where
        s_ = case wp1 of
            Just (i, _) -> clearBit s (fromIntegral i)
            _ -> s
        s' = case wp2 of
            Just (i, _) -> setBit s_ (fromIntegral i)
            _ -> s_
        a1 = testBit s (fromIntegral i1)
        a2 = testBit s (fromIntegral i2)
        a3 = testBit s (fromIntegral i3)
        a4 = testBit s (fromIntegral i4)

selectReadRes :: (Bool, Unsigned 5, RegValue, RegValue, WritePort, WritePort) -> RegValue
selectReadRes (_, 0, _, _, _, _) = 0
selectReadRes (_, i, _, _, _, Just (i', v)) | i == unpack i' = v
selectReadRes (_, i, _, _, Just (i', v), _) | i == unpack i' = v
selectReadRes (False, _, a, _, _, _) = a
selectReadRes (True, _, _, b, _, _) = b