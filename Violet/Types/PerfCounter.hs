module Violet.Types.PerfCounter where

import Clash.Prelude

data PerfCounters = PerfCounters {
    perfCycles :: BitVector 64,
    perfInstRet :: BitVector 64
} deriving (Generic, NFDataX)

initPerfCounters = PerfCounters {
    perfCycles = 0,
    perfInstRet = 0
}

data InstRetire = InstRetire (BitVector 2)
    deriving (Generic, NFDataX)
