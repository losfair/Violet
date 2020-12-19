module Violet.IP.StaticDM where

import Clash.Prelude

import Violet.Types.DCache
import Violet.Types.Ctrl
import qualified Debug.Trace
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Issue as IssueT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Pipe as PipeT

type ReadPort = MemAddr
type WritePort = Maybe (MemAddr, MemData, WriteMask)

type ReadMini = (Selector, SignExtension)

data StaticDM = StaticDM
    deriving (Generic, NFDataX)

instance DCacheImpl StaticDM where
    issueAccess _ req weCommit fastBusIn = (commitPort, weReq, fastBusOut)
        where
            -- stage 1
            pipelinedAddrPort = fmap transformPipelinedAddrPort req
            writePort = fmap transformWritePort req

            -- stage 2
            (rawReadResult, ramProviderReady, fastBusOut) = mkRam pipelinedAddrPort req writeCommitPort fastBusIn
            readMini = register Nothing (fmap transformReadMini req)
            delayedPC = register 0 (fmap transformPC req)
            delayedRd = register 0 (fmap transformRd req)
            delayedPipelinedAddrPort = register 0 pipelinedAddrPort
            forwardedReadResult = writeForward delayedPipelinedAddrPort rawReadResult writePortFinal
            readResult = fmap transformReadResult $ bundle (forwardedReadResult, readMini)
            commitPort = fmap transformCommitPort $ bundle (delayedPC, delayedRd, readResult, writePortD1, delayedPipelinedAddrPort, ramProviderReady)
            writePortD1 = register Nothing writePort

            -- Commit stage doesn't handle DCache exception and write disable in the same cycle.
            -- So we need to handle it here.
            writePortD1Gated = fmap f $ bundle (writePortD1, commitPort)
                where
                    f (wp, cp) = case cp of
                        PipeT.Exc _ -> Nothing
                        _ -> wp
            
            -- stage 3
            writePortFinal = register Nothing writePortD1Gated
            weReq = register NoWrite (fmap transformWeReq writePortD1Gated)
            writeCommitPort = fmap transformWriteCommit $ bundle (writePortFinal, weCommit)

            -- Pipelined r/w address port.
            transformPipelinedAddrPort req = case req of
                Just (_, addr, _) -> addr
                _ -> 0

            transformWritePort req = case req of
                Just (_, addr, WriteAccess (v, mask)) -> Just (addr, v, mask)
                _ -> Nothing
            transformReadMini req = case req of
                Just (_, _, ReadAccess (_, sel, ext)) -> Just (sel, ext)
                _ -> Nothing
            transformPC req = case req of
                Just (pc, _, _) -> pc
                _ -> 0
            transformRd req = case req of
                Just (_, _, ReadAccess (i, _, _)) -> i
                _ -> 0
            transformReadResult :: ((BitVector 8, BitVector 8, BitVector 8, BitVector 8), Maybe ReadMini) -> Maybe (BitVector 32)
            transformReadResult (_, Nothing) = Nothing
            transformReadResult ((ram3, ram2, ram1, ram0), Just (sel, signExt)) = Just r
                where
                    ext8 = case signExt of
                        UseZeroExtend -> zeroExtend
                        UseSignExtend -> signExtend
                    ext16 = case signExt of
                        UseZeroExtend -> zeroExtend
                        UseSignExtend -> signExtend
                    r = case sel of
                        SelByte0 -> ext8 ram0
                        SelByte1 -> ext8 ram1
                        SelByte2 -> ext8 ram2
                        SelByte3 -> ext8 ram3
                        SelHalf0 -> ext16 (ram1 ++# ram0)
                        SelHalf1 -> ext16 (ram3 ++# ram2)
                        SelWord -> ram3 ++# ram2 ++# ram1 ++# ram0
            transformCommitPort (pc, rd, readRes, writePort, readPort, ramProviderReady) = case (readRes, writePort, readPort, ramProviderReady) of
                (_, Just (waddr, wdata, _), _, False) -> PipeT.Exc (pc, PipeT.EarlyExc $ PipeT.IOMemWrite pc waddr wdata)
                (Just _, _, raddr, False) -> PipeT.Exc (pc, PipeT.EarlyExc $ PipeT.IOMemRead pc rd raddr)
                (Just x, _, _, _) -> PipeT.Ok (pc, Just (PipeT.GPR rd x), Nothing)
                (_, Just _, _, _) -> PipeT.Ok (pc, Nothing, Nothing)
                _ -> PipeT.Bubble
            transformWriteCommit (writePort, weCommit) = case weCommit of
                CanWrite -> writePort
                NoWrite -> Nothing
            transformWeReq (Just _) = CanWrite
            transformWeReq Nothing = NoWrite

writeForward :: HiddenClockResetEnable dom
             => Signal dom ReadPort
             -> Signal dom (BitVector 8, BitVector 8, BitVector 8, BitVector 8)
             -> Signal dom WritePort
             -> Signal dom (BitVector 8, BitVector 8, BitVector 8, BitVector 8)
writeForward rp input wp = fmap forwardOne $ bundle (wp, rp, input)
    where
        forwardOne (Nothing, rp, x) = x
        forwardOne ((Just (waddr, wdata, wmask)), rp, (b3, b2, b1, b0)) = if valid then (nb3, nb2, nb1, nb0) else (b3, b2, b1, b0)
            where
                valid = slice d31 d2 waddr == slice d31 d2 rp
                nb0 = if testBit wmask 0 then slice d7 d0 wdata else b0
                nb1 = if testBit wmask 1 then slice d15 d8 wdata else b1
                nb2 = if testBit wmask 2 then slice d23 d16 wdata else b2
                nb3 = if testBit wmask 3 then slice d31 d24 wdata else b3

mkRam :: HiddenClockResetEnable dom
      => Signal dom ReadPort
      -> Signal dom (Maybe (FetchT.PC, MemAddr, Access))
      -> Signal dom WritePort
      -> Signal dom FastBusIn
      -> (
          Signal dom (BitVector 8, BitVector 8, BitVector 8, BitVector 8),
          Signal dom Bool,
          Signal dom FastBusOut
          )
mkRam pipelinedAddrPort req writeCommit fastBusIn = (values, readiness, busOut)
    where
        values = bundle (
            slice d31 d24 . iFastData <$> fastBusIn,
            slice d23 d16 . iFastData <$> fastBusIn,
            slice d15 d8 . iFastData <$> fastBusIn,
            slice d7 d0 . iFastData <$> fastBusIn
            )
        readiness = iFastReady <$> fastBusIn
        busOut = mkBusOut <$> bundle (pipelinedAddrPort, req, writeCommit)

        mkBusOut (pipelinedAddrPort, req, writeCommit) = FastBusOut {
            oFastValid = fastValid, oFastWrite = fastWrite, oFastAddr = fastAddr,
            oFastWrValid = fastWrValid, oFastWrAddr = fastWrAddr, oFastWrData = fastWrData, oFastWrMask = fastWrMask
            }
            where
                (fastValid, fastWrite, fastAddr) = case req of
                    Just (_, addr, WriteAccess (v, mask)) -> (True, True, addr)
                    Just (_, addr, ReadAccess _) -> (True, False, addr)
                    _ -> (False, undefined, undefined)
                (fastWrValid, fastWrAddr, fastWrData, fastWrMask) = case writeCommit of
                    Just (addr, wdata, mask) -> (True, addr, wdata, mask)
                    _ -> (False, undefined, undefined, undefined)
