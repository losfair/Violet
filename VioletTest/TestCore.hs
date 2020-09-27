module VioletTest.TestCore where

import Clash.Prelude
import Text.Printf
import Data.Char
import qualified Data.List
import qualified Debug.Trace as Trace
import qualified Prelude
import qualified Violet.Gen.CoreGen
import qualified Violet.Types.Commit as CommitT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Ctrl as CtrlT
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
        (beCmd, commitLog, fifoPushCap, sysOut) = unbundle $ Violet.Backend.Wiring.wiring Violet.IP.StaticDM.StaticDM frontendOut sysIn
        sysIn = sysbusProvider sysOut

sysbusProvider :: HiddenClockResetEnable dom
               => Signal dom CtrlT.SystemBusOut
               -> Signal dom CtrlT.SystemBusIn
sysbusProvider = mealy sysbusProvider' (False, emptySystemBusIn)

sysbusProvider' :: (Bool, CtrlT.SystemBusIn)
                -> CtrlT.SystemBusOut
                -> ((Bool, CtrlT.SystemBusIn), CtrlT.SystemBusIn)
sysbusProvider' (active, sysIn) sysOut = ((active', sysIn'), sysIn)
    where
        oIoBus = CtrlT.oIoBus sysOut
        (active', sysIn') = case active of
            True -> (if CtrlT.oIoValid oIoBus then True else False, emptySystemBusIn)
            False -> (if CtrlT.oIoValid oIoBus then True else False, CtrlT.SystemBusIn { CtrlT.iIoBus = ioBus' })

        ioBus' = case CtrlT.oIoValid oIoBus of
            True -> case CtrlT.oIoAddr oIoBus of
                0xfe000000 -> case CtrlT.oIoWrite oIoBus of
                    True -> Trace.trace ("Putchar: " Prelude.++ (show $ chr (fromIntegral (CtrlT.oIoData oIoBus)))) CtrlT.IOBusIn { CtrlT.iIoReady = True, CtrlT.iIoData = undefined }
                    False -> undefined -- read not allowed
                _ -> Trace.trace ("bad io address: " Prelude.++ (show $ CtrlT.oIoAddr oIoBus)) undefined -- bad address
            False -> emptyIOBusIn

emptySystemBusIn = CtrlT.SystemBusIn {
    CtrlT.iIoBus = emptyIOBusIn
}

emptyIOBusIn = CtrlT.IOBusIn { CtrlT.iIoReady = False, CtrlT.iIoData = undefined }