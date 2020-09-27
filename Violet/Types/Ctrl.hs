module Violet.Types.Ctrl where

import Clash.Prelude

data CtrlBusy = Busy | Idle
    deriving (Generic, NFDataX, Eq, Show)

type CsrIndex = BitVector 8

data SystemBusOut = SystemBusOut {
    oIoBus :: IOBusOut
} deriving (Generic, NFDataX)

data SystemBusIn = SystemBusIn {
    iIoBus :: IOBusIn
} deriving (Generic, NFDataX)

data IOBusOut = IOBusOut {
    oIoValid :: Bool,
    oIoWrite :: Bool,
    oIoAddr :: BitVector 32,
    oIoData :: BitVector 32
} deriving (Generic, NFDataX)

data IOBusIn = IOBusIn {
    iIoReady :: Bool,
    iIoData :: BitVector 32
} deriving (Generic, NFDataX)

idleSystemBusOut = SystemBusOut {
    oIoBus = idleIOBusOut
}

idleIOBusOut = IOBusOut {
    oIoValid = False,
    oIoWrite = undefined,
    oIoAddr = undefined,
    oIoData = undefined
}
