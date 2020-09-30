module Violet.Backend.PerfCounter where

import Clash.Prelude
import Violet.Types.PerfCounter

perfCounter :: HiddenClockResetEnable dom
            => Signal dom InstRetire
            -> Signal dom BranchStat
            -> Signal dom PerfCounters
perfCounter instRetire branchStat = c
    where
        c = register initPerfCounters (fmap update $ bundle (c, instRetire, branchStat))

update :: (PerfCounters, InstRetire, BranchStat) -> PerfCounters
update (current, InstRetire instRetire, branchStat) = PerfCounters {
    perfCycles = perfCycles current + 1,
    perfInstRet = perfInstRet current + zeroExtend instRetire,
    perfBranchHits = perfBranchHits current + if branchStat == BranchHit then 1 else 0,
    perfBranchMisses = perfBranchMisses current + if branchStat == BranchMiss then 1 else 0
}
