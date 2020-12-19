module Violet.Gen.CoreGen where

import Clash.Prelude
import qualified Violet.Backend.Wiring
import qualified Violet.Frontend.Wiring
import qualified Violet.IP.StaticDM
import qualified Violet.IP.StaticIM
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Commit as CommitT
import qualified Violet.Types.Ctrl as CtrlT

violetCore :: Clock XilinxSystem
          -> Reset XilinxSystem
          -> Enable XilinxSystem
          -> Signal XilinxSystem CtrlT.SystemBusIn
          -> Signal XilinxSystem (CommitT.CommitLog, CtrlT.SystemBusOut)
violetCore = exposeClockResetEnable violetCore'

violetCore' :: HiddenClockResetEnable dom
            => Signal dom CtrlT.SystemBusIn
            -> Signal dom (CommitT.CommitLog, CtrlT.SystemBusOut)
violetCore' sysIn = bundle (commitLog, sysOut)
    where
        frontendOut = Violet.Frontend.Wiring.wiring Violet.IP.StaticIM.StaticIM beCmd fifoPushCap historyUpd
        (beCmd, commitLog, fifoPushCap, sysOut, historyUpd) = unbundle $ Violet.Backend.Wiring.wiring Violet.IP.StaticDM.StaticDM frontendOut sysIn

{-# ANN violetCore
    (Synthesize {
        t_name = "VioletCore",
        t_inputs = [
            PortName "clk",
            PortName "rst",
            PortName "en",
            PortProduct "sysbus_i" [
                PortName "fast",
                PortName "io"
            ]
        ],
        t_output = PortProduct "" [
            PortName "commit",
            PortProduct "sysbus_o" [
                PortName "fast",
                PortName "io"
            ]
        ]
    })
    #-}
