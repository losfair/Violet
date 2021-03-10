module Violet.Frontend.BHT where

import Clash.Prelude
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Gpr as GprT

import qualified Violet.Config as Config
import qualified Violet.TypeLevel.Nat

type IndexBits = $(Violet.TypeLevel.Nat.natT Config.gshareSizeBits)

data Entry = Entry {
    taken :: Vec (2^FetchT.GlobalHistoryBits) (BitVector 2)
} deriving (Generic, NFDataX, Show)

emptyEntry = Entry { taken = repeat 0b10 }

bht :: HiddenClockResetEnable dom
    => Signal dom FetchT.BackendCmd
    -> Signal dom (Maybe FetchT.HistoryUpdate)
    -> Signal dom FetchT.PC
    -> Signal dom FetchT.PC
    -> Signal dom FetchT.GlobalHistory
    -> Signal dom (Maybe Bool, Maybe Bool)
bht cmd historyUpd prevPC1 prevPC2 ghistory = bundle (result1, result2)
    where
        bufferIndex1 = fmap mkBufferIndex prevPC1
        delayedPrevPC1 = register 0 prevPC1
        bufferOut1 = readNew (blockRam1 NoClearOnReset (SNat :: SNat (2^IndexBits)) emptyEntry) bufferIndex1 bufferWrite
        result1 = fmap mkOut $ bundle (delayedPrevPC1, ghistory, bufferOut1)

        bufferIndex2 = fmap mkBufferIndex prevPC2
        delayedPrevPC2 = register 0 prevPC2
        bufferOut2 = readNew (blockRam1 NoClearOnReset (SNat :: SNat (2^IndexBits)) emptyEntry) bufferIndex2 bufferWrite
        result2 = fmap mkOut $ bundle (delayedPrevPC2, ghistory, bufferOut2)

        bufferIndexPreload = fmap mkBufferPreload $ bundle (cmd, historyUpd)
        preloadOut = readNew (blockRam1 NoClearOnReset (SNat :: SNat (2^IndexBits)) emptyEntry) bufferIndexPreload bufferWrite

        delayedCmd = register FetchT.NoCmd cmd
        delayedHistoryUpd = register Nothing historyUpd
        bufferWrite = fmap mkBufferWrite $ bundle (delayedCmd, delayedHistoryUpd, preloadOut)

mkBufferIndex :: FetchT.PC -> Unsigned IndexBits
mkBufferIndex = unpack . slice (SNat :: SNat (1 + IndexBits)) d2

mkBufferPreload :: (FetchT.BackendCmd, Maybe FetchT.HistoryUpdate) -> Unsigned IndexBits
mkBufferPreload x = case x of
    (FetchT.ApplyBranch (_, (prev, _)), _) -> mkBufferIndex prev
    (_, Just upd) -> mkBufferIndex (FetchT.hFrom upd)
    _ -> 0

mkBufferWrite :: (FetchT.BackendCmd, Maybe FetchT.HistoryUpdate, Entry) -> Maybe (Unsigned IndexBits, Entry)
mkBufferWrite (cmd, upd, current) = case (cmd, upd) of
    (FetchT.ApplyBranch (_, (prev, pref)), _) -> r
        where
            r = case pref of
                FetchT.Taken (FetchT.GlobalHistory history) ->
                    Just (mkBufferIndex prev, current { taken = replace (mkGshareIndex prev history) (boundedAdd (taken current !! (mkGshareIndex prev history)) 1) (taken current) })
                FetchT.NotTaken (FetchT.GlobalHistory history) ->
                    Just (mkBufferIndex prev, current { taken = replace (mkGshareIndex prev history) (boundedSub (taken current !! (mkGshareIndex prev history)) 1) (taken current) })
                FetchT.NoPref -> Nothing
    (_, Just upd) -> r
        where
            prev = FetchT.hFrom upd
            FetchT.GlobalHistory history = FetchT.hHistory upd
            r = case FetchT.hTaken upd of
                True ->
                    Just (mkBufferIndex prev, current { taken = replace (mkGshareIndex prev history) (boundedAdd (taken current !! (mkGshareIndex prev history)) 1) (taken current) })
                False ->
                    Just (mkBufferIndex prev, current { taken = replace (mkGshareIndex prev history) (boundedSub (taken current !! (mkGshareIndex prev history)) 1) (taken current) })
    _ -> Nothing

mkOut :: (FetchT.PC, FetchT.GlobalHistory, Entry) -> Maybe Bool
mkOut (pc, FetchT.GlobalHistory historyIndex, entry) =
    case taken entry !! (mkGshareIndex pc historyIndex) of
        0b00 -> Just False
        0b11 -> Just True
        _ -> Nothing

mkGshareIndex :: FetchT.PC -> BitVector FetchT.GlobalHistoryBits -> BitVector FetchT.GlobalHistoryBits
mkGshareIndex pc h = xor (slice (SNat :: SNat (1 + FetchT.GlobalHistoryBits)) d2 pc) h
