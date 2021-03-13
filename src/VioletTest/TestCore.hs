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
import qualified Violet.IP.DirectMappedICache
import qualified Violet.Frontend.Wiring
import qualified Violet.Backend.Wiring
import qualified System.IO
import qualified System.IO.Unsafe
import qualified Violet.Trace as T

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
        icacheInst = Violet.IP.DirectMappedICache.issueAccess icRefillIn
        frontendOut = Violet.Frontend.Wiring.wiring icacheInst beCmd fifoPushCap historyUpd
        (beCmd, commitLog, fifoPushCap, sysOut, historyUpd, icRefillIn) = unbundle $ Violet.Backend.Wiring.wiring Violet.IP.StaticDM.StaticDM frontendOut sysIn
        sysIn = sysbusProvider sysOut

type CycleCounter = BitVector 64

sysbusProvider :: HiddenClockResetEnable dom
               => Signal dom CtrlT.SystemBusOut
               -> Signal dom CtrlT.SystemBusIn
sysbusProvider sysOut = bundleSysbus <$> bundle (ioIn, icRefillIn, icRefillReadyIn, fastIn)
    where
        ioIn = (mealy iobusProvider (False, emptyIOBusIn, 0)) ioOut
        (icRefillIn, icRefillReadyIn) = unbundle $ icRefillProvider icRefillOut
        fastIn = fastbusProvider fastOut

        (ioOut, icRefillOut, fastOut) = unbundle $ unbundleSysbus <$> sysOut
        unbundleSysbus x = (CtrlT.oIoBus x, CtrlT.oIcRefill x, CtrlT.oFastBus x)
        bundleSysbus (io, icRefillIn, icRefillReadyIn, fastIn) =
            CtrlT.SystemBusIn {
                CtrlT.iIoBus = io,
                CtrlT.iIcRefill = icRefillIn,
                CtrlT.iIcRefillReady = icRefillReadyIn,
                CtrlT.iFastBus = fastIn
            }

fastbusProvider :: HiddenClockResetEnable dom
    => Signal dom CtrlT.FastBusOut
    -> Signal dom CtrlT.FastBusIn
fastbusProvider fastOut = pure (CtrlT.FastBusIn { CtrlT.iFastReady = True, CtrlT.iFastData = 0 })

icRefillProvider :: HiddenClockResetEnable dom
    => Signal dom CtrlT.IcRefillOut
    -> Signal dom (CtrlT.IcRefillIn, Bool)
icRefillProvider refillOut = bundle (refillIn, ready)
    where
        readValue = blockRamFilePow2 "im.txt" readAddr (pure Nothing)
        refillIn = f <$> bundle (counter, lastReadAddr, readValue)
            where
                f (counter, lastReadAddr, readValue) =
                    CtrlT.IcRefillIn {
                        CtrlT.iIcRefillValid = counter /= Nothing,
                        CtrlT.iIcRefillAddr = pack lastReadAddr,
                        CtrlT.iIcRefillData = readValue
                    }

        ready = register False $ f <$> bundle (ready, counter, refillOut)
            where
                f :: (Bool, Maybe (Index 8), CtrlT.IcRefillOut) -> Bool
                f (ready, counter, refillOut) =
                    case (ready, CtrlT.oIcRefillValid refillOut) of
                        (False, _) -> counter == Just maxBound
                        (True, False) -> False
                        (True, True) -> True
        counter = register Nothing nextCounter
        nextCounter = f <$> bundle (counter, refillOut, ready)
            where
                f :: (Maybe (Index 8), CtrlT.IcRefillOut, Bool) -> Maybe (Index 8)
                f (counter, refillOut, ready) =
                    case counter of
                        Just x -> if x == maxBound then Nothing else Just (x + 1)
                        Nothing -> if CtrlT.oIcRefillValid refillOut && not ready then Just 0 else Nothing
        lastReadAddr = register undefined readAddr
        readAddr = f <$> bundle (refillOut, nextCounter)
            where
                f :: (CtrlT.IcRefillOut, Maybe (Index 8)) -> Unsigned 32
                f (_, Nothing) = undefined
                f (refillOut, Just nextCounter) = unpack (CtrlT.oIcRefillAddr refillOut + shiftL (fromIntegral nextCounter :: BitVector 32) 2)


iobusProvider :: (Bool, CtrlT.IOBusIn, CycleCounter)
    -> CtrlT.IOBusOut
    -> ((Bool, CtrlT.IOBusIn, CycleCounter), CtrlT.IOBusIn)
iobusProvider (active, ioIn, cycles) ioOut = ((active', ioIn', cycles + 1), ioIn)
    where
        (active', ioIn') = case active of
            True -> (if CtrlT.oIoValid ioOut then True else False, emptyIOBusIn)
            False -> (if CtrlT.oIoValid ioOut then True else False, ioBus')

        ioBus' = case CtrlT.oIoValid ioOut of
            True -> case CtrlT.oIoAddr ioOut of
                0xfe000000 -> case CtrlT.oIoWrite ioOut of
                    True -> System.IO.Unsafe.unsafePerformIO $ doPutChar (chr (fromIntegral (CtrlT.oIoData ioOut)))
                    False -> undefined -- read not allowed
                0xfe000010 -> case CtrlT.oIoWrite ioOut of
                    True -> undefined -- write not allowed
                    False -> CtrlT.IOBusIn { CtrlT.iIoReady = True, CtrlT.iIoData = slice d31 d0 cycles }
                0xfe000014 -> case CtrlT.oIoWrite ioOut of
                    True -> undefined -- write not allowed
                    False -> CtrlT.IOBusIn { CtrlT.iIoReady = True, CtrlT.iIoData = slice d63 d32 cycles }
                _ -> Trace.trace ("bad io address: " Prelude.++ (show $ CtrlT.oIoAddr ioOut)) undefined -- bad address
            False -> emptyIOBusIn

emptySystemBusIn = CtrlT.SystemBusIn {
    CtrlT.iIoBus = emptyIOBusIn
}

emptyIOBusIn = CtrlT.IOBusIn { CtrlT.iIoReady = False, CtrlT.iIoData = undefined }

doPutChar :: Char -> Prelude.IO CtrlT.IOBusIn
doPutChar x = do
    System.IO.hPutChar System.IO.stderr x
    return CtrlT.IOBusIn { CtrlT.iIoReady = True, CtrlT.iIoData = undefined }
