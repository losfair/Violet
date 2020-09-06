import Clash.Prelude

topEntity :: Clock System -> Reset System -> Enable System
    -> Signal System (BitVector 32)
    -> Signal System (BitVector 32)
topEntity = exposeClockResetEnable orangeCore

orangeCore :: HiddenClockResetEnable dom
    => Signal dom (BitVector 32)
    -> Signal dom (BitVector 32)
orangeCore x = fmap (\x -> x + 1) x
