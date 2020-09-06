module Orange.Core where

import Clash.Prelude
import qualified Orange.Memory.DBus as DBus
import qualified Orange.Memory.IBus as IBus
import qualified Orange.Interrupt as Interrupt
import qualified Orange.Memory.ICache as ICache
import qualified Orange.Interlock as Interlock

core :: HiddenClockResetEnable dom
    => Signal dom IBus.IBusIn
    -> Signal dom DBus.DBusIn
    -> Signal dom Interrupt.InterruptInterface
    -> Signal dom (IBus.IBusOut, DBus.DBusOut)
core ibusIn dbusIn intrIn = pure (IBus.IBusOut {}, DBus.DBusOut {})