module Orange.Types.Fifo where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.DecodeDep as DecodeDep

type FifoBits = 3 :: Nat
data FifoPushCap = CanPush | WillFull
    deriving (Generic, NFDataX)
data FifoPopReq = PopNothing | PopOne | PopTwo
    deriving (Generic, NFDataX)
data FifoItem = Item DecodeDep.DecodeDepBundle | Bubble
    deriving (Generic, NFDataX)
