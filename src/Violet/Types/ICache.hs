module Violet.Types.ICache where

import Clash.Prelude
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT

class ICacheImpl a where
    issueAccess :: HiddenClockResetEnable dom
            => a
            -> Signal dom FetchT.PC
            -> Signal dom FifoT.FifoPushCap
            -> Signal dom (Maybe (BitVector 64))
