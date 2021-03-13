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
import qualified System.IO
import qualified System.IO.Unsafe

test :: Prelude.IO ()
test = f l
    where
        f :: [((FifoT.FifoItem, FifoT.FifoItem), CommitT.CommitLog)] -> Prelude.IO ()
        f (x:xs) = do
            putStrLn $ showCommitLog $ snd x
            f xs
        l = sample_lazy (runCore' :: Signal System ((FifoT.FifoItem, FifoT.FifoItem), CommitT.CommitLog))

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
        frontendOut = Violet.Frontend.Wiring.wiring Violet.IP.StaticIM.issueAccess beCmd fifoPushCap historyUpd
        (beCmd, commitLog, fifoPushCap, sysOut, historyUpd, icRefillIn) = unbundle $ Violet.Backend.Wiring.wiring Violet.IP.StaticDM.StaticDM frontendOut sysIn
        sysIn = sysbusProvider sysOut

type CycleCounter = BitVector 64

sysbusProvider :: HiddenClockResetEnable dom
               => Signal dom CtrlT.SystemBusOut
               -> Signal dom CtrlT.SystemBusIn
sysbusProvider = mealy sysbusProvider' (False, emptySystemBusIn, 0)

sysbusProvider' :: (Bool, CtrlT.SystemBusIn, CycleCounter)
                -> CtrlT.SystemBusOut
                -> ((Bool, CtrlT.SystemBusIn, CycleCounter), CtrlT.SystemBusIn)
sysbusProvider' (active, sysIn, cycles) sysOut = ((active', sysIn', cycles + 1), sysIn)
    where
        oIoBus = CtrlT.oIoBus sysOut
        (active', sysIn') = case active of
            True -> (if CtrlT.oIoValid oIoBus then True else False, emptySystemBusIn)
            False -> (if CtrlT.oIoValid oIoBus then True else False, CtrlT.SystemBusIn { CtrlT.iIoBus = ioBus' })

        ioBus' = case CtrlT.oIoValid oIoBus of
            True -> case CtrlT.oIoAddr oIoBus of
                0xfe000000 -> case CtrlT.oIoWrite oIoBus of
                    True -> System.IO.Unsafe.unsafePerformIO $ doPutChar (chr (fromIntegral (CtrlT.oIoData oIoBus)))
                    False -> undefined -- read not allowed
                0xfe000010 -> case CtrlT.oIoWrite oIoBus of
                    True -> undefined -- write not allowed
                    False -> CtrlT.IOBusIn { CtrlT.iIoReady = True, CtrlT.iIoData = slice d31 d0 cycles }
                0xfe000014 -> case CtrlT.oIoWrite oIoBus of
                    True -> undefined -- write not allowed
                    False -> CtrlT.IOBusIn { CtrlT.iIoReady = True, CtrlT.iIoData = slice d63 d32 cycles }
                _ -> Trace.trace ("bad io address: " Prelude.++ (show $ CtrlT.oIoAddr oIoBus)) undefined -- bad address
            False -> emptyIOBusIn

emptySystemBusIn = CtrlT.SystemBusIn {
    CtrlT.iIoBus = emptyIOBusIn
}

emptyIOBusIn = CtrlT.IOBusIn { CtrlT.iIoReady = False, CtrlT.iIoData = undefined }

doPutChar :: Char -> Prelude.IO CtrlT.IOBusIn
doPutChar x = do
    System.IO.hPutChar System.IO.stderr x
    return CtrlT.IOBusIn { CtrlT.iIoReady = True, CtrlT.iIoData = undefined }
