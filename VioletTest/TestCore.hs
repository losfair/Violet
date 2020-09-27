module VioletTest.TestCore where

import Clash.Prelude
import Text.Printf
import qualified Data.List
import qualified Prelude
import qualified Violet.Gen.CoreGen
import qualified Violet.Types.Commit as CommitT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.IP.StaticIM
import qualified Violet.IP.StaticDM
import qualified Violet.Frontend.Wiring
import qualified Violet.Backend.Wiring

test :: Prelude.IO ()
test = do
    Prelude.putStrLn $ Data.List.intercalate "\n" $ Prelude.map (showCommitLog . snd) $ sampleN 3000 (runCore' :: Signal System ((FifoT.FifoItem, FifoT.FifoItem), CommitT.CommitLog))

showCommitLog :: CommitT.CommitLog -> Prelude.String
showCommitLog log = showPort (CommitT.pc1 log, CommitT.writePort1 log) Prelude.++ " " Prelude.++ showPort (CommitT.pc2 log, CommitT.writePort2 log)

showPort :: (Maybe FetchT.PC, GprT.WritePort) -> Prelude.String
showPort (Nothing, _) = "(bubble)"
showPort (Just pc, wp) = printf "[0x%08x]" (fromIntegral pc :: Int) Prelude.++ showWritePort wp

showWritePort :: GprT.WritePort -> Prelude.String
showWritePort Nothing = "<no_write>"
showWritePort (Just (i, v)) = printf "<write:%d=0x%08x>" (fromIntegral i :: Int) (fromIntegral v :: Int)

runCore' :: HiddenClockResetEnable dom
        => Signal dom ((FifoT.FifoItem, FifoT.FifoItem), CommitT.CommitLog)
runCore' = bundle (frontendOut, commitLog)
    where
        frontendOut = Violet.Frontend.Wiring.wiring Violet.IP.StaticIM.StaticIM beCmd fifoPushCap
        (beCmd, commitLog, fifoPushCap) = unbundle $ Violet.Backend.Wiring.wiring Violet.IP.StaticDM.StaticDM frontendOut