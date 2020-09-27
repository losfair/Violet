module VioletTest.TestBackend where

import Clash.Prelude
import qualified Data.List
import qualified Prelude
import qualified Violet.Backend.Wiring
import qualified Violet.IP.StaticDM
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Commit as CommitT
import qualified Violet.Types.DecodeDep as DepT
import qualified Violet.Frontend.DecodeDep as DecodeDep

test :: Prelude.IO ()
test = do
    Prelude.putStrLn $ Data.List.intercalate "\n" $ Prelude.map Prelude.show $ sampleN 20 (hideClockResetEnable makeStream)

makeStream :: Clock System -> Reset System -> Enable System -> Signal System (FetchT.BackendCmd, CommitT.CommitLog, FifoT.FifoPushCap)
makeStream clk rst en = output
    where
        inputs = [
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x00010000, 0x02a00093, FetchT.validMetadata), (0x00010004, 0x00100133, FetchT.validMetadata)),
            ((0x00010008, 0x00208293, FetchT.validMetadata), (0x0001000c, 0x00508333, FetchT.validMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata)),
            ((0x0, FetchT.nopInst, FetchT.emptyMetadata), (0x0, FetchT.nopInst, FetchT.emptyMetadata))
            ]
        inputsS = fromList inputs
        decoded = (exposeClockResetEnable $ DecodeDep.decodeDep $ fmap (\((a, b), c) -> (a, b, c)) $ bundle (inputsS, fifoPushCap)) clk rst en
        output = (exposeClockResetEnable $ Violet.Backend.Wiring.wiring Violet.IP.StaticDM.StaticDM decoded) clk rst en
        (_, _, fifoPushCap) = unbundle output
