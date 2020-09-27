module Violet.Frontend.Wiring where

import Clash.Prelude
import qualified Violet.Frontend.ICache
import qualified Violet.Frontend.DecodeDep
import qualified Violet.Frontend.PC
import qualified Violet.Frontend.BTB

import qualified Violet.Types.ICache as ICacheT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT

wiring :: HiddenClockResetEnable dom
       => ICacheT.ICacheImpl a
       => a
       -> Signal dom FetchT.BackendCmd
       -> Signal dom FifoT.FifoPushCap
       -> Signal dom (FifoT.FifoItem, FifoT.FifoItem)
wiring icacheImpl beCmd pushCap = decodeOut
    where
        pcOut = Violet.Frontend.PC.pc beCmd (bundle (pdCmd, pdAck)) pushCap
        (pcVal, _) = unbundle pcOut
        btbPrediction = Violet.Frontend.BTB.btb beCmd pcVal

        (pdCmd, pdAck, decodePorts) = unbundle $ Violet.Frontend.ICache.icache icacheImpl pcOut btbPrediction pushCap
        (decodePort1, decodePort2) = unbundle decodePorts
        decodeOut = Violet.Frontend.DecodeDep.decodeDep $ bundle (decodePort1, decodePort2, pushCap)
