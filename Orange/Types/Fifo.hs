module Orange.Types.Fifo where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.DecodeDep as DecodeDep

type FifoBits = 4 :: Nat
data FifoPushCap = CanPush | WillFull
    deriving (Generic, NFDataX, Eq, Show)
data FifoPopReq = PopNothing | PopOne | PopTwo
    deriving (Generic, NFDataX, Eq)
data FifoItem = Item DecodeDep.DecodeDepBundle | Bubble
    deriving (Generic, NFDataX, Show)

isBubble :: FifoItem -> Bool
isBubble (Item _) = False
isBubble Bubble = True

gatedRegister :: HiddenClockResetEnable dom
              => NFDataX a
              => a
              -> Signal dom FifoPushCap
              -> Signal dom a
              -> Signal dom a
gatedRegister initial pushCap input = reg
    where
        reg = register initial (fmap gate $ bundle (pushCap, input, reg))
        gate (pushCap, input, reg) = case pushCap of
            CanPush -> input
            WillFull -> reg