module Orange.Gen.CoreGen where

import Clash.Prelude
import qualified Orange.Backend.Wiring
import qualified Orange.Frontend.Wiring
import qualified Orange.IP.StaticDM
import qualified Orange.IP.StaticIM
import qualified Orange.Types.Fifo as FifoT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Commit as CommitT
import qualified Orange.Frontend.DecodeDep as DecodeDep

orangeCore :: Clock XilinxSystem
          -> Reset XilinxSystem
          -> Enable XilinxSystem
          -> Signal XilinxSystem CommitT.CommitLog
orangeCore = exposeClockResetEnable orangeCore'

orangeCore' :: HiddenClockResetEnable dom
            => Signal dom CommitT.CommitLog
orangeCore' = commitLog
    where
        frontendOut = Orange.Frontend.Wiring.wiring Orange.IP.StaticIM.StaticIM beCmd fifoPushCap
        (beCmd, commitLog, fifoPushCap) = unbundle $ Orange.Backend.Wiring.wiring Orange.IP.StaticDM.StaticDM frontendOut

{-# ANN orangeCore
    (Synthesize {
        t_name = "OrangeCore",
        t_inputs = [
            PortName "clk",
            PortName "rst",
            PortName "en",
            PortProduct "fetch" [PortName "port1", PortName "port2"]
        ],
        t_output = PortName "commit"
    })
    #-}
