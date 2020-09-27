module Violet.Types.Ctrl where

import Clash.Prelude

data CtrlBusy = Busy | Idle
    deriving (Generic, NFDataX, Eq, Show)

type CsrIndex = BitVector 8
