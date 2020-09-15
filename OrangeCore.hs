import Clash.Prelude
import Orange.Config
import qualified Orange.Backend.Fifo
import qualified Orange.Frontend.DecodeDep
import qualified Orange.Backend.Issue
import qualified Orange.Backend.Gpr
import qualified Orange.Backend.Pipe
import qualified Orange.Backend.Bypass
import qualified Orange.Backend.IntAlu
import qualified Orange.Backend.Branch
import qualified Orange.Backend.Commit
import qualified Orange.Backend.DCache
import qualified Orange.Backend.Wiring
import qualified Orange.IP.StaticDM
import qualified Orange.IP.StaticIM
import qualified Orange.Gen.CoreGen
import qualified Orange.Frontend.ICache
import qualified Orange.Frontend.PC
import qualified Orange.Frontend.Wiring

topEntity :: Clock System -> Reset System -> Enable System
    -> Signal System Bool
topEntity = exposeClockResetEnable $ pure False
