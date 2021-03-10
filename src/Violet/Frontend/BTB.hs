module Violet.Frontend.BTB where

import Clash.Prelude
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Gpr as GprT

import qualified Violet.Config as Config
import qualified Violet.TypeLevel.Nat

type BufferBits = $(Violet.TypeLevel.Nat.natT Config.btbSizeBits)

btb :: HiddenClockResetEnable dom
    => Signal dom FetchT.BackendCmd
    -> Signal dom FetchT.PC
    -> Signal dom FetchT.PC
btb cmd prevPC = bufferOut
    where
        bufferIndex = fmap mkBufferIndex prevPC
        bufferOut = blockRamPow2 (repeat (0 :: BitVector 32)) bufferIndex bufferWrite
        bufferWrite = fmap mkBufferWrite cmd

mkBufferIndex :: FetchT.PC -> Unsigned BufferBits
mkBufferIndex = unpack . slice (SNat :: SNat (2 + BufferBits)) d3

mkBufferWrite :: FetchT.BackendCmd -> Maybe (Unsigned BufferBits, FetchT.PC)
mkBufferWrite cmd = case cmd of
    FetchT.ApplyBranch (new, (prev, _)) -> Just (mkBufferIndex prev, new)
    _ -> Nothing
