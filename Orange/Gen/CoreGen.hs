module Orange.Gen.CoreGen where

import Clash.Prelude
import qualified Orange.Backend.Wiring
import qualified Orange.IP.StaticDM
import qualified Orange.Types.Fifo as FifoT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Commit as CommitT
import qualified Orange.Frontend.DecodeDep as DecodeDep

orangeCore :: Clock XilinxSystem
          -> Reset XilinxSystem
          -> Enable XilinxSystem
          -> Signal XilinxSystem ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata))
          -> Signal XilinxSystem CommitT.CommitLog
orangeCore = exposeClockResetEnable orangeCore'

orangeCore' :: HiddenClockResetEnable dom
            => Signal dom ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata))
            -> Signal dom CommitT.CommitLog
orangeCore' fetchInput = commitLog
    where
        decoded = DecodeDep.decodeDep $ fmap (\((a, b), c) -> (a, b, c)) $ bundle (fetchInput, fifoPushCap)
        (_, commitLog, fifoPushCap) = unbundle $ Orange.Backend.Wiring.wiring Orange.IP.StaticDM.StaticDM decoded

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
