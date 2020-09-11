module Orange.Types.Memory where

import Clash.Prelude

data MemoryAck = MemAck | NoMemAck
    deriving (Generic, NFDataX, Eq)
