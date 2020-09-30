module Violet.IP.DMultiWay where

import Clash.Prelude

import Violet.Types.DCache
import Data.Maybe
import qualified Debug.Trace
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Issue as IssueT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Pipe as PipeT
import qualified Violet.Types.Ctrl as CtrlT

type DataMemBits = 10 :: Nat -- in words
type LineBits = 3 :: Nat -- line size in words
type TagMemBits = DataMemBits - LineBits -- tag memory size in entries
type TagBits = 30 - DataMemBits -- tag bits
type Ways = 4 :: Nat -- number of ways

data DMultiWay = DMultiWay
    deriving (Generic, NFDataX)

data RefillState = NotRefilling
    | WaitingForTag MemAddr
    | WritingBack (BitVector (30 - LineBits)) (Unsigned LineBits) MemAddr
    | WaitingForWord (BitVector (30 - LineBits)) (Unsigned LineBits) Bool
    deriving (Generic, NFDataX)

instance DCacheImpl DMultiWay where
    issueAccess _ req weCommit refill dbusIn = (commit, weReq, refillCompletion, pure CtrlT.idleDBusOut)
        where
            -- stage 1
            accTag = fmap extractTag req
            accTagMemIndex = fmap extractTagMemIndex req
            accDataMemIndex = fmap extractDataMemIndex req
            accPC = fmap extractPC req
            accAddr = fmap extractAddr req
            (accReadRd, accReadSel, accReadSext) = unbundle $ fmap extractReadReq req
            accWriteReq = fmap extractWriteReq req
            accWriteReqM = fmap extractWriteReqM req

            -- entering stage 2 - access memories.
            resTag = map (\wp -> mkTagMem accTagMemIndex wp) $ unbundle wpTag
            resData = map (\wp -> mkDataMem accDataMemIndex wp) $ unbundle wpData

            -- our part of stage 2.
            accTagD1 = register undefined accTag
            accWay = fmap lookupTag $ bundle (bundle resTag, accTagD1)

            -- read postprocessing
            accPCD1 = register undefined accPC
            accAddrD1 = register undefined accAddr
            accDataMemIndexD1 = register undefined accDataMemIndex
            accReadRdD1 = register undefined accReadRd
            accReadSelD1 = register undefined accReadSel
            accReadSextD1 = register undefined accReadSext
            rCommit = fmap selectReadResult $ bundle (accPCD1, accAddrD1, accReadRdD1, accReadSelD1, accReadSextD1, accWay, bundle resData, accDataMemIndexD1, accDataMemIndexD2, accWriteReqMD2)

            -- write postprocessing
            wCommit = fmap selectWriteResult $ bundle (accPCD1, accAddrD1, accWay)

            -- stage 3: write.
            -- delay one more stage since we need to wait for committing
            accDataMemIndexD2 = register undefined $ register undefined accDataMemIndex
            accWriteReqD2 = register undefined $ register undefined accWriteReq
            accWriteReqMD2 = register Nothing $ register Nothing accWriteReqM
            accWayD1 = register undefined $ fmap fromJust accWay
            wpDataPipelined = fmap mkWp $ bundle (weCommit, accWayD1, accDataMemIndex, accWriteReqD2)
            wpData = fmap mkRefillLayout (bundle (wpDataRefill, refillIndex, wpDataPipelined))
            wpTag = fmap mkRefillLayout (bundle (wpTagRefill, refillIndex, pure (repeat Nothing)))
            refillIndex = pure 0 :: Signal dom (Index Ways)

            -- mux things together
            hadReadAccess = register False $ fmap checkReadAccess req
            hadWriteAccess = register False $ fmap checkWriteAccess req
            (commit, weReq) = unbundle $ fmap selectCommit $ bundle (hadReadAccess, hadWriteAccess, rCommit, wCommit)

            -- Duplicate memories once to avoid one more arbitration on the read port.
            -- XXX: Should we change this?
            resTagRefiller = map (\wp -> mkTagMem refillerTagMemIndex wp) $ unbundle wpTag
            resDataRefiller = map (\wp -> mkDataMem refillerDataMemIndex wp) $ unbundle wpData

            -- refill state machine
            -- refillCompletion = pure RefillNotCompleted
            (refillCompletion, wpDataRefill, wpTagRefill, refillerTagMemIndex, refillerDataMemIndex, dbusOut) = unbundle $ mealy refill' NotRefilling (bundle (refill, dbusIn, resTagRefiller, resDataRefiller))

{-|
refill' :: RefillState
        -> (Maybe Refill, CtrlT.DBusIn, Maybe (BitVector TagBits), MemData)
        -> (
            RefillState,
            (
                RefillCompletion,
                Maybe (Unsigned DataMemBits, MemData, WriteMask),
                Maybe (Unsigned TagMemBits, Maybe (BitVector TagBits)),
                Unsigned TagMemBits,
                Unsigned DataMemBits,
                CtrlT.DBusOut
            )
            )
refill' NotRefilling (Nothing, _, _, _) = (NotRefilling, (RefillNotCompleted, Nothing, Nothing, undefined, undefined, CtrlT.idleDBusOut))
refill' NotRefilling (Just (Refill addr), _, _, _) = (WaitingForTag addr, (RefillNotCompleted, Nothing, Nothing, extractTagMemIndexR addr, undefined, CtrlT.idleDBusOut))
refill' (WaitingForTag addr) (_, _, tag, _) =
    where
        linePrefix = slice d31 (SNat :: SNat (2 + LineBits)) addr)
        s' = case tag of
            Just _ -> WritingBack (tag ++) 0 addr
            Nothing -> (
                WaitingForWord ( 0 True,
                (
                    RefillNotCompleted,
                    Nothing,
                    Nothing,
                    CtrlT.DBusOut { oDBusValid = True, oDBusWrite = False, oDBusAddr = base ++# (pack (i + 1)) ++# (0b00 :: BitVector 2), oDBusData = undefined }
                )
            )
    (
        WaitingForWord (slice d31 (SNat :: SNat (2 + LineBits)) addr) 0 True,
        (
            RefillNotCompleted,
            Nothing,
            Nothing,
            
        )
    )

refill' (WaitingForWord base i _) (_, dbus) | CtrlT.iDBusReady dbus = (s', (comp, Just wd, wt))
    where
        s' = if i == maxBound then NotRefilling else WaitingForWord base (i + 1) False
        comp = if i == maxBound then RefillCompleted else RefillNotCompleted
        fullAddr = base ++# (pack i) ++# (0b00 :: BitVector 2)
        wd = (extractDataMemIndexR fullAddr, CtrlT.iDBusData dbus, 0b1111)
        wt = if i == maxBound then Just (extractTagMemIndexR fullAddr, Just $ extractTagR fullAddr) else Nothing
        dbusOut =
            if i == maxBound then
                CtrlT.idleDBusOut 
            else
                CtrlT.DBusOut { oDBusValid = True, oDBusWrite = False, oDBusAddr = base ++# (pack (i + 1)) ++# (0b00 :: BitVector 2), oDBusData = undefined }
refill' (WaitingForWord base i _) _ = (WaitingForWord base i False, (RefillNotCompleted, Nothing, Nothing, CtrlT.idleDBusOut))
|-}

mkWp :: (WriteEnable, Index Ways, Unsigned DataMemBits, (MemData, WriteMask))
     -> Vec Ways (Maybe (Unsigned DataMemBits, MemData, WriteMask))
mkWp (CanWrite, i, addr, (d, m)) = map f $ iterateI (+1) 0
    where
        f i' = if i' == i then Just (addr, d, m) else Nothing
mkWp _ = repeat Nothing

mkRefillLayout :: (Maybe a, Index Ways, Vec Ways (Maybe a)) -> Vec Ways (Maybe a)
mkRefillLayout (Nothing, _, alt) = alt
mkRefillLayout (Just x, i, _) = map f $ iterateI (+1) 0
    where
        f i' = if i' == i then Just x else Nothing

selectReadResult :: (
                        FetchT.PC, MemAddr,
                        GprT.RegIndex, Selector, SignExtension,
                        Maybe (Index Ways), Vec Ways MemData,
                        Unsigned DataMemBits, Unsigned DataMemBits, Maybe (MemData, WriteMask)
                    ) -> PipeT.Commit
selectReadResult (pc, addr, _, _, _, Nothing, _, _, _, _) = PipeT.Exc (pc, PipeT.EarlyExc $ PipeT.DCacheRefill pc addr)
selectReadResult (pc, _, rd, sel, signExt, Just i, d, fwDIndex, fwWIndex, fwWp) = PipeT.Ok (pc, Just (PipeT.GPR rd fwOut), Nothing)
    where
        v = transformReadResult (d !! i, sel, signExt)
        fwOut = writeForward fwDIndex v fwWIndex fwWp

selectWriteResult :: (FetchT.PC, MemAddr, Maybe (Index Ways)) -> (PipeT.Commit, WriteEnable)
selectWriteResult (pc, addr, Nothing) = (PipeT.Exc (pc, PipeT.EarlyExc $ PipeT.DCacheRefill pc addr), NoWrite)
selectWriteResult (pc, _, _) = (PipeT.Ok (pc, Nothing, Nothing), CanWrite)

selectCommit :: (Bool, Bool, PipeT.Commit, (PipeT.Commit, WriteEnable)) -> (PipeT.Commit, WriteEnable)
selectCommit (True, _, rCommit, _) = (rCommit, NoWrite)
selectCommit (_, True, _, (wCommit, weReq)) = (wCommit, weReq)
selectCommit _ = (PipeT.Bubble, NoWrite)

checkReadAccess :: Maybe (FetchT.PC, MemAddr, Access) -> Bool
checkReadAccess (Just (_, _, ReadAccess _)) = True
checkReadAccess _ = False

checkWriteAccess :: Maybe (FetchT.PC, MemAddr, Access) -> Bool
checkWriteAccess (Just (_, _, WriteAccess _)) = True
checkWriteAccess _ = False

extractReadReq :: Maybe (FetchT.PC, MemAddr, Access) -> (GprT.RegIndex, Selector, SignExtension)
extractReadReq (Just (_, _, ReadAccess (rd, sel, sext))) = (rd, sel, sext)
extractReadReq _ = undefined

writeForward :: Unsigned DataMemBits
             -> BitVector 32
             -> Unsigned DataMemBits
             -> Maybe (MemData, WriteMask)
             -> BitVector 32
writeForward rp rv waddr (Just (wdata, wmask)) = if valid then nrv else rv
    where
        valid = waddr == rp
        nb0 = slice d7 d0 $ if testBit wmask 0 then wdata else rv
        nb1 = slice d15 d8 $ if testBit wmask 1 then wdata else rv
        nb2 = slice d23 d16 $ if testBit wmask 2 then wdata else rv
        nb3 = slice d31 d24 $ if testBit wmask 3 then wdata else rv
        nrv = nb3 ++# nb2 ++# nb1 ++# nb0
writeForward _ rv _ _ = rv

lookupTag :: (Vec Ways (Maybe (BitVector TagBits)), BitVector TagBits) -> Maybe (Index Ways)
lookupTag (v, t) = fold f (imap m v)
    where
        m i (Just x) | x == t = Just i
        m _ _ = Nothing
        f (Just x) _ = Just x
        f _ (Just x) = Just x
        f _ _ = Nothing

extractPC :: Maybe (FetchT.PC, MemAddr, Access) -> FetchT.PC
extractPC (Just (pc, _, _)) = pc
extractPC _ = undefined

extractAddr :: Maybe (FetchT.PC, MemAddr, Access) -> MemAddr
extractAddr (Just (_, addr, _)) = addr
extractAddr _ = undefined

extractWriteReq :: Maybe (FetchT.PC, MemAddr, Access) -> (MemData, WriteMask)
extractWriteReq = fromJust . extractWriteReqM

extractWriteReqM :: Maybe (FetchT.PC, MemAddr, Access) -> Maybe (MemData, WriteMask)
extractWriteReqM  (Just (_, _, WriteAccess (v, m))) = Just (v, m)
extractWriteReqM _ = Nothing

extractTag :: Maybe (FetchT.PC, MemAddr, Access) -> BitVector TagBits
extractTag (Just (_, addr, _)) = slice d31 (SNat :: SNat (DataMemBits + 2)) addr
extractTag _ = undefined

extractTagR :: MemAddr -> BitVector TagBits
extractTagR addr = slice d31 (SNat :: SNat (DataMemBits + 2)) addr

extractTagMemIndex :: Maybe (FetchT.PC, MemAddr, Access) -> Unsigned TagMemBits
extractTagMemIndex (Just (_, addr, _)) = unpack $ slice (SNat :: SNat (DataMemBits + 1)) (SNat :: SNat (LineBits + 2)) addr
extractTagMemIndex _ = undefined

extractTagMemIndexR :: MemAddr -> Unsigned TagMemBits
extractTagMemIndexR addr = unpack $ slice (SNat :: SNat (DataMemBits + 1)) (SNat :: SNat (LineBits + 2)) addr

extractDataMemIndex ::  Maybe (FetchT.PC, MemAddr, Access) -> Unsigned DataMemBits
extractDataMemIndex (Just (_, addr, _)) = unpack $ slice (SNat :: SNat (DataMemBits + 1)) (SNat :: SNat 2) addr
extractDataMemIndex _ = undefined

extractDataMemIndexR ::  MemAddr -> Unsigned DataMemBits
extractDataMemIndexR addr = unpack $ slice (SNat :: SNat (DataMemBits + 1)) (SNat :: SNat 2) addr

transformReadResult :: (BitVector 32, Selector, SignExtension) -> BitVector 32
transformReadResult (r, sel, signExt) = result
    where
        ext8 = case signExt of
            UseZeroExtend -> zeroExtend
            UseSignExtend -> signExtend
        ext16 = case signExt of
            UseZeroExtend -> zeroExtend
            UseSignExtend -> signExtend
        result = case sel of
            SelByte0 -> ext8 (slice d7 d0 r)
            SelByte1 -> ext8 (slice d15 d8 r)
            SelByte2 -> ext8 (slice d23 d16 r)
            SelByte3 -> ext8 (slice d31 d24 r)
            SelHalf0 -> ext16 (slice d15 d0 r)
            SelHalf1 -> ext16 (slice d31 d16 r)
            SelWord -> r

mkTagMem :: HiddenClockResetEnable dom
         => Signal dom (Unsigned TagMemBits)
         -> Signal dom (Maybe (Unsigned TagMemBits, Maybe (BitVector TagBits)))
         -> Signal dom (Maybe (BitVector TagBits))
mkTagMem rp wp = readNew (blockRamPow2 (repeat Nothing)) rp wp

mkDataMem :: HiddenClockResetEnable dom
          => Signal dom (Unsigned DataMemBits)
          -> Signal dom (Maybe (Unsigned DataMemBits, MemData, WriteMask))
          -> Signal dom MemData
mkDataMem readPort writePort = fmap concatBytes $ bundle (ram3, ram2, ram1, ram0)
    where
        ram0 = mkByteRam (slice d7 d0) (\x -> testBit x 0) readPort writePort
        ram1 = mkByteRam (slice d15 d8) (\x -> testBit x 1) readPort writePort
        ram2 = mkByteRam (slice d23 d16) (\x -> testBit x 2) readPort writePort
        ram3 = mkByteRam (slice d31 d24) (\x -> testBit x 3) readPort writePort
        concatBytes (b3, b2, b1, b0) = b3 ++# b2 ++# b1 ++# b0

mkByteRam :: HiddenClockResetEnable dom
          => (BitVector 32 -> BitVector 8)
          -> (WriteMask -> Bool)
          -> Signal dom (Unsigned DataMemBits)
          -> Signal dom (Maybe (Unsigned DataMemBits, MemData, WriteMask))
          -> Signal dom (BitVector 8)
mkByteRam getRange getWe readPort writePort = readResult
    where
        rawWrite = fmap extractWrite writePort
        readResult = readNew (blockRamPow2 (repeat undefined)) readPort rawWrite
        extractWrite x = case x of
            Just (addr, v, mask) -> if getWe mask then Just (addr, getRange v) else Nothing
            _ -> Nothing
