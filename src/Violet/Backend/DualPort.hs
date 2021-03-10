-- Generic 2W1R dual port async RAM abstraction.

module Violet.Backend.DualPort where

import Clash.Prelude
import Violet.Types.Gpr
import qualified Violet.Types.Fifo as FifoT

dpram :: HiddenClockResetEnable dom
    => KnownNat n
    => NFDataX b
    => Signal dom (Unsigned n)
    -> Signal dom (Maybe (Unsigned n, b))
    -> Signal dom (Maybe (Unsigned n, b))
    -> Signal dom b
dpram raddr wp1 wp2 = readRes
    where
        readA = asyncRamPow2 raddr wp1
        readB = asyncRamPow2 raddr wp2
        a = (mealy mkAllocation 0) (bundle (wp1, wp2, raddr))
        readRes = fmap selectReadRes $ bundle (a, raddr, readA, readB)

mkAllocation :: KnownNat n
             => NFDataX b
             => BitVector (2^n)
             -> (Maybe (Unsigned n, b), Maybe (Unsigned n, b), Unsigned n)
             -> (BitVector (2^n), Bool)
mkAllocation s (wp1, wp2, i) = (s', a)
    where
        s_ = case wp1 of
            Just (i, _) -> clearBit s (fromIntegral i)
            _ -> s
        s' = case wp2 of
            Just (i, _) -> setBit s_ (fromIntegral i)
            _ -> s_
        a = testBit s (fromIntegral i)

selectReadRes :: KnownNat n
              => NFDataX b
              => (Bool, Unsigned n, b, b)
              -> b
selectReadRes (False, _, x, _) = x
selectReadRes (True, _, _, y) = y