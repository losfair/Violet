module Orange.Frontend.Wiring where

import Clash.Prelude
import qualified Orange.Frontend.ICache
import qualified Orange.Frontend.DecodeDep
import qualified Orange.Frontend.PC
import qualified Orange.Frontend.BTB

import qualified Orange.Types.ICache as ICacheT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Fifo as FifoT

wiring :: HiddenClockResetEnable dom
       => ICacheT.ICacheImpl a
       => a
       -> Signal dom FetchT.BackendCmd
       -> Signal dom FifoT.FifoPushCap
       -> Signal dom (FifoT.FifoItem, FifoT.FifoItem)
wiring icacheImpl beCmd pushCap = decodeOut
    where
        pcOut = Orange.Frontend.PC.pc beCmd (bundle (pdCmd, pdAck)) pushCap
        (pcVal, _) = unbundle pcOut
        btbPrediction = Orange.Frontend.BTB.btb beCmd pcVal

        (pdCmd, pdAck, decodePorts) = unbundle $ Orange.Frontend.ICache.icache icacheImpl pcOut btbPrediction pushCap
        (decodePort1, decodePort2) = unbundle decodePorts
        decodeOut = Orange.Frontend.DecodeDep.decodeDep $ bundle (decodePort1, decodePort2, pushCap)
