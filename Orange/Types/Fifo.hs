module Orange.Types.Fifo where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.DecodeDep as DecodeDep

type FifoBits = 3 :: Nat
data FifoPushCap = CanPush | WillFull
    deriving (Generic, NFDataX, Eq, Show)
data FifoPopReq = PopNothing | PopOne | PopTwo
    deriving (Generic, NFDataX, Eq)
data FifoItem = Item DecodeDep.DecodeDepBundle | Bubble
    deriving (Generic, NFDataX)

isBubble :: FifoItem -> Bool
isBubble (Item _) = False
isBubble Bubble = True
