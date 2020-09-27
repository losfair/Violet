module Violet.Interrupt where

import Clash.Prelude

data InterruptInterface = InterruptInterface {
    timerInterrupt :: Bool,
    softwareInterrupt :: Bool,
    externalInterrupt :: Bool
}
