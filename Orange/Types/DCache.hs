module Orange.Types.DCache where

import Clash.Prelude

data WriteEnable = CanWrite | NoWrite
type WriteMask = BitVector 4