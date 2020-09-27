module Violet.Gen.CoreGen where

import Clash.Prelude
import qualified Violet.Backend.Wiring
import qualified Violet.Frontend.Wiring
import qualified Violet.IP.StaticDM
import qualified Violet.IP.StaticIM
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Commit as CommitT
import qualified Violet.Frontend.DecodeDep as DecodeDep

violetCore :: Clock XilinxSystem
          -> Reset XilinxSystem
          -> Enable XilinxSystem
          -> Signal XilinxSystem CommitT.CommitLog
violetCore = exposeClockResetEnable violetCore'

violetCore' :: HiddenClockResetEnable dom
            => Signal dom CommitT.CommitLog
violetCore' = commitLog
    where
        frontendOut = Violet.Frontend.Wiring.wiring Violet.IP.StaticIM.StaticIM beCmd fifoPushCap
        (beCmd, commitLog, fifoPushCap) = unbundle $ Violet.Backend.Wiring.wiring Violet.IP.StaticDM.StaticDM frontendOut

{-# ANN violetCore
    (Synthesize {
        t_name = "VioletCore",
        t_inputs = [
            PortName "clk",
            PortName "rst",
            PortName "en",
            PortProduct "fetch" [PortName "port1", PortName "port2"]
        ],
        t_output = PortName "commit"
    })
    #-}
