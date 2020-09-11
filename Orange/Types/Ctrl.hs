module Orange.Types.Ctrl where

import Clash.Prelude

data CtrlAck = CtrlAck | NoCtrlAck
    deriving (Generic, NFDataX, Eq)
