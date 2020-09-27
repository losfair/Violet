module Violet.Core where

import Clash.Prelude
import qualified Violet.Memory.DBus as DBus
import qualified Violet.Memory.IBus as IBus
import qualified Violet.Interrupt as Interrupt
import qualified Violet.Memory.ICache as ICache
import qualified Violet.Interlock as Interlock

core :: HiddenClockResetEnable dom
    => Signal dom IBus.IBusIn
    -> Signal dom DBus.DBusIn
    -> Signal dom Interrupt.InterruptInterface
    -> Signal dom (IBus.IBusOut, DBus.DBusOut)
core ibusIn dbusIn intrIn = pure (IBus.IBusOut {}, DBus.DBusOut {})