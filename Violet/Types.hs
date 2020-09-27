module Violet.Types where

import Clash.Prelude

type ByteAddress = BitVector 32
type MemoryWord = BitVector 32

data MemoryAccessWidth = ByteAccess | HalfWordAccess | WordAccess
