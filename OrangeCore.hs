import Clash.Prelude
import Orange.Config
import qualified Orange.Core
import qualified Orange.Memory.DBus as DBus
import qualified Orange.Memory.IBus as IBus
import qualified Orange.Interrupt as Interrupt

topEntity :: Clock System -> Reset System -> Enable System
    -> Signal System IBus.IBusIn
    -> Signal System DBus.DBusIn
    -> Signal System Interrupt.InterruptInterface
    -> Signal System (IBus.IBusOut, DBus.DBusOut)
topEntity = exposeClockResetEnable Orange.Core.core
