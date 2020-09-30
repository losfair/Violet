module Violet.Backend.PerfCounter where

import Clash.Prelude
import Violet.Types.PerfCounter

perfCounter :: HiddenClockResetEnable dom
            => Signal dom InstRetire
            -> Signal dom PerfCounters
perfCounter instRetire = c
    where
        c = register initPerfCounters (fmap update $ bundle (c, instRetire))

update :: (PerfCounters, InstRetire) -> PerfCounters
update (current, InstRetire instRetire) = PerfCounters {
    perfCycles = perfCycles current + 1,
    perfInstRet = perfInstRet current + zeroExtend instRetire
}
