module Violet.Types.PerfCounter where

import Clash.Prelude

data PerfCounters = PerfCounters {
    perfCycles :: BitVector 64,
    perfInstRet :: BitVector 64,
    perfBranchHits :: BitVector 64,
    perfBranchMisses :: BitVector 64
} deriving (Generic, NFDataX)

initPerfCounters = PerfCounters {
    perfCycles = 0,
    perfInstRet = 0,
    perfBranchHits = 0,
    perfBranchMisses = 0
}

data InstRetire = InstRetire (BitVector 2)
    deriving (Generic, NFDataX)

data BranchStat = BranchHit | BranchMiss | BranchNone
    deriving (Generic, NFDataX, Eq)
