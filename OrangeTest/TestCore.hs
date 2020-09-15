module OrangeTest.TestCore where

import Clash.Prelude
import Text.Printf
import qualified Data.List
import qualified Prelude
import qualified Orange.Gen.CoreGen
import qualified Orange.Types.Commit as CommitT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Fifo as FifoT
import qualified Orange.IP.StaticIM
import qualified Orange.IP.StaticDM
import qualified Orange.Frontend.Wiring
import qualified Orange.Backend.Wiring

test :: Prelude.IO ()
test = do
    Prelude.putStrLn $ Data.List.intercalate "\n" $ Prelude.map (showCommitLog . snd) $ sampleN 200 (runCore' :: Signal System ((FifoT.FifoItem, FifoT.FifoItem), CommitT.CommitLog))

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
        frontendOut = Orange.Frontend.Wiring.wiring Orange.IP.StaticIM.StaticIM beCmd fifoPushCap
        (beCmd, commitLog, fifoPushCap) = unbundle $ Orange.Backend.Wiring.wiring Orange.IP.StaticDM.StaticDM frontendOut