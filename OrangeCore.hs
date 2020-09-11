import Clash.Prelude
import Orange.Config
import qualified Orange.Fifo
import qualified Orange.Frontend.DecodeDep
import qualified Orange.Backend.Issue
import qualified Orange.Backend.Gpr
import qualified Orange.Backend.Pipe
import qualified Orange.Backend.Bypass
import qualified Orange.Backend.IntAlu

topEntity :: Clock System -> Reset System -> Enable System
    -> Signal System Bool
topEntity = exposeClockResetEnable $ pure False
