module Violet.Gen.BackendGen where

import Clash.Prelude
import qualified Violet.Backend.Wiring
import qualified Violet.IP.StaticDM
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Commit as CommitT

wiringGen :: Clock XilinxSystem
          -> Reset XilinxSystem
          -> Enable XilinxSystem
          -> Signal XilinxSystem (FifoT.FifoItem, FifoT.FifoItem)
          -> Signal XilinxSystem (FetchT.BackendCmd, CommitT.CommitLog, FifoT.FifoPushCap)
wiringGen = exposeClockResetEnable $ Violet.Backend.Wiring.wiring Violet.IP.StaticDM.StaticDM

{-# ANN wiringGen
    (Synthesize {
        t_name = "backend_wiring",
        t_inputs = [
            PortName "clk",
            PortName "rst",
            PortName "en",
            PortProduct "fifo" [PortName "port1", PortName "port2"]
        ],
        t_output = PortProduct "out" [PortName "backend_cmd", PortName "commit_log", PortName "fifo_push_cap"]
    })
    #-}
