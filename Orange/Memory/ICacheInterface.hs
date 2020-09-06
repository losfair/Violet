module Orange.Memory.ICacheInterface where

import Clash.Prelude
import Orange.Types

data FetchIn a = FetchIn {
    inAddress :: ByteAddress,
    inAux :: a
} deriving (Generic, NFDataX)

data FetchOut a = FetchOut {
    outData :: MemoryWord,
    outAux :: a
} deriving (Generic, NFDataX)
