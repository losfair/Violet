module Orange.Types.Gpr where

import Clash.Prelude

type RegIndex = BitVector 5
type RegValue = BitVector 32

type WritePort = Maybe (RegIndex, RegValue)
