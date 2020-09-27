module Violet.Memory.IBus where

import Clash.Prelude
import Violet.Types

data IBusIn = IBusIn {
    inReady :: Bool,
    inData :: MemoryWord
} deriving (Generic, NFDataX)

data IBusOut = IBusOut {
    outValid :: Bool,
    outAddress :: MemoryWord
} deriving (Generic, NFDataX)

emptyOut :: IBusOut
emptyOut = IBusOut { outValid = False, outAddress = 0 }
