module Orange.Gen.BackendGen where

import Clash.Prelude
import qualified Orange.Backend.Wiring
import qualified Orange.IP.StaticDM
import qualified Orange.Types.Fifo as FifoT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Commit as CommitT

wiringGen :: Clock XilinxSystem
          -> Reset XilinxSystem
          -> Enable XilinxSystem
          -> Signal XilinxSystem (FifoT.FifoItem, FifoT.FifoItem)
          -> Signal XilinxSystem (FetchT.BackendCmd, CommitT.CommitLog, FifoT.FifoPushCap)
wiringGen = exposeClockResetEnable $ Orange.Backend.Wiring.wiring Orange.IP.StaticDM.StaticDM

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
