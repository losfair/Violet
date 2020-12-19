module Violet.Types.Ctrl where

import Clash.Prelude

data CtrlBusy = Busy | Idle
    deriving (Generic, NFDataX, Eq, Show)

data SystemBusOut = SystemBusOut {
    oFastBus :: FastBusOut,
    oIoBus :: IOBusOut
} deriving (Generic, NFDataX)

data SystemBusIn = SystemBusIn {
    iFastBus :: FastBusIn,
    iIoBus :: IOBusIn
} deriving (Generic, NFDataX)

data FastBusOut = FastBusOut {
    -- Pipelined interface. Must be side-effect free.
    oFastValid :: Bool,
    oFastWrite :: Bool,
    oFastAddr :: BitVector 32,

    -- Writes are delayed by one cycle.
    oFastWrValid :: Bool,
    oFastWrAddr :: BitVector 32,
    oFastWrData :: BitVector 32,
    oFastWrMask :: BitVector 4
} deriving (Generic, NFDataX)

data IOBusOut = IOBusOut {
    oIoValid :: Bool,
    oIoWrite :: Bool,
    oIoAddr :: BitVector 32,
    oIoData :: BitVector 32
} deriving (Generic, NFDataX)

data FastBusIn = FastBusIn {
    iFastReady :: Bool,
    iFastData :: BitVector 32
} deriving (Generic, NFDataX)

data IOBusIn = IOBusIn {
    iIoReady :: Bool,
    iIoData :: BitVector 32
} deriving (Generic, NFDataX)

idleSystemBusOut = SystemBusOut {
    oFastBus = idleFastBusOut,
    oIoBus = idleIOBusOut
}

idleIOBusOut = IOBusOut {
    oIoValid = False,
    oIoWrite = undefined,
    oIoAddr = undefined,
    oIoData = undefined
}

idleFastBusOut = FastBusOut {
    oFastValid = False,
    oFastWrite = undefined,
    oFastAddr = undefined,
    oFastWrValid = False,
    oFastWrAddr = undefined,
    oFastWrData = undefined,
    oFastWrMask = undefined
}
