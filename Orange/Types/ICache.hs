module Orange.Types.ICache where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Fifo as FifoT

class ICacheImpl a where
    issueAccess :: HiddenClockResetEnable dom
            => a
            -> Signal dom FetchT.PC
            -> Signal dom FifoT.FifoPushCap
            -> Signal dom (Maybe (BitVector 64))