module Orange.IP.StaticDM where

import Clash.Prelude

import Orange.Types.DCache
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Issue as IssueT
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Pipe as PipeT

type RamBits = 14 :: Nat
type ReadPort = MemAddr
type WritePort = Maybe (MemAddr, MemData, WriteMask)

type ReadMini = (Selector, SignExtension)

data StaticDM = StaticDM
    deriving (Generic, NFDataX)

instance DCacheImpl StaticDM where
    issueAccess _ req weCommit = (commitPort, weReq)
        where
            -- stage 1
            readPort = fmap transformReadPort req
            writePort = fmap transformWritePort req

            -- stage 2
            rawReadResult = mkRam readPort writeCommitPort
            readMini = register Nothing (fmap transformReadMini req)
            delayedPC = register 0 (fmap transformPC req)
            delayedRd = register 0 (fmap transformRd req)
            readResult = fmap transformReadResult $ bundle (rawReadResult, readMini)
            commitPort = fmap transformCommitPort $ bundle (delayedPC, delayedRd, readResult)
            writePortD1 = register Nothing writePort
            
            -- stage 3
            writePortFinal = register Nothing writePortD1
            weReq = register NoWrite (fmap transformWeReq writePortD1)
            writeCommitPort = fmap transformWriteCommit $ bundle (writePortFinal, weCommit)

            transformReadPort req = case req of
                Just (_, addr, ReadAccess (_, _, _)) -> addr
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
            transformCommitPort (pc, rd, readRes) = case readRes of
                Just x -> PipeT.Ok (pc, Just (PipeT.GPR rd x))
                Nothing -> PipeT.Bubble
            transformWriteCommit (writePort, weCommit) = case weCommit of
                CanWrite -> writePort
                NoWrite -> Nothing
            transformWeReq (Just _) = CanWrite
            transformWeReq Nothing = NoWrite

mkRam :: HiddenClockResetEnable dom
      => Signal dom ReadPort
      -> Signal dom WritePort
      -> Signal dom (BitVector 8, BitVector 8, BitVector 8, BitVector 8)
mkRam readPort writePort = bundle (ram3, ram2, ram1, ram0)
    where
        ram0 = mkByteRam (slice d7 d0) (\x -> testBit x 0) readPort writePort
        ram1 = mkByteRam (slice d15 d8) (\x -> testBit x 1) readPort writePort
        ram2 = mkByteRam (slice d23 d16) (\x -> testBit x 2) readPort writePort
        ram3 = mkByteRam (slice d31 d24) (\x -> testBit x 3) readPort writePort

mkByteRam :: HiddenClockResetEnable dom
          => (BitVector 32 -> BitVector 8)
          -> (WriteMask -> Bool)
          -> Signal dom MemAddr
          -> Signal dom (Maybe (MemAddr, MemData, WriteMask))
          -> Signal dom (BitVector 8)
mkByteRam getRange getWe readPort writePort = readResult
    where
        rawAddr = fmap ramIndex readPort
        rawWrite = fmap extractWrite writePort
        readResult = blockRamPow2 (repeat 0) rawAddr rawWrite
        extractWrite x = case x of
            Just (addr, v, mask) -> if getWe mask then Just (ramIndex addr, getRange v) else Nothing
            _ -> Nothing

ramIndex :: MemAddr
         -> Unsigned RamBits
ramIndex x = unpack $ slice (SNat :: SNat (2 + RamBits - 1)) (SNat :: SNat 2) x