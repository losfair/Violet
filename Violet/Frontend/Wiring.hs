module Violet.Frontend.Wiring where

import Clash.Prelude
import qualified Violet.Frontend.ICache
import qualified Violet.Frontend.PC
import qualified Violet.Frontend.BTB
import qualified Violet.Frontend.BHT

import qualified Violet.Types.ICache as ICacheT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT

import qualified Violet.Config as Config

wiring :: HiddenClockResetEnable dom
       => ICacheT.ICacheImpl a
       => a
       -> Signal dom FetchT.BackendCmd
       -> Signal dom FifoT.FifoPushCap
       -> Signal dom (Maybe FetchT.HistoryUpdate)
       -> Signal dom (FifoT.FifoItem, FifoT.FifoItem)
wiring icacheImpl beCmd pushCap historyUpd = issuePorts
    where
        pcOut = Violet.Frontend.PC.pc beCmd (bundle (pdCmd, pdAck)) pushCap
        (pcVal, _) = unbundle pcOut
        btbPrediction = if Config.enableBTB then Violet.Frontend.BTB.btb beCmd pcVal else pure 0
        bhtPrediction = if Config.enableBHT then Violet.Frontend.BHT.bht beCmd historyUpd pcVal (fmap (\x -> setBit x 2) pcVal) ghistory else pure (Nothing, Nothing)

        (pdCmd, pdAck, issuePorts, ghistory) = unbundle $ Violet.Frontend.ICache.icache icacheImpl pcOut btbPrediction bhtPrediction pushCap
