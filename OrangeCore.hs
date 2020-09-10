import Clash.Prelude
import Orange.Config
import qualified Orange.Fifo
import qualified Orange.Frontend.DecodeDep
-- import qualified Orange.Backend.Issue

topEntity :: Clock System -> Reset System -> Enable System
    -> Signal System Bool
topEntity = exposeClockResetEnable $ pure False
