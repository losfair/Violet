module Violet.Frontend.BHT where

import Clash.Prelude
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Gpr as GprT

type IndexBits = 8 :: Nat

data Entry = Entry {
    fromPC :: BitVector (30 - IndexBits),
    taken :: Vec (2^FetchT.GlobalHistoryBits) (BitVector 2)
} deriving (Generic, NFDataX, Show)

emptyEntry = Entry { fromPC = 0, taken = repeat 0b11 }

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
        bufferOut1 = readNew (blockRamPow2 (repeat emptyEntry)) bufferIndex1 bufferWrite
        result1 = fmap mkOut $ bundle (delayedPrevPC1, ghistory, bufferOut1)

        bufferIndex2 = fmap mkBufferIndex prevPC2
        delayedPrevPC2 = register 0 prevPC2
        bufferOut2 = readNew (blockRamPow2 (repeat emptyEntry)) bufferIndex2 bufferWrite
        result2 = fmap mkOut $ bundle (delayedPrevPC2, ghistory, bufferOut2)

        bufferIndexPreload = fmap mkBufferPreload $ bundle (cmd, historyUpd)
        preloadOut = readNew (blockRamPow2 (repeat emptyEntry)) bufferIndexPreload bufferWrite

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
                    if fromPC current == mkTag prev then
                        Just (mkBufferIndex prev, current { taken = replace history (boundedAdd (taken current !! history) 1) (taken current) })
                    else
                        Just (mkBufferIndex prev, Entry { fromPC = mkTag prev, taken = repeat 0b11 })
                FetchT.NotTaken (FetchT.GlobalHistory history) ->
                    if fromPC current == mkTag prev then
                        Just (mkBufferIndex prev, current { taken = replace history (boundedSub (taken current !! history) 1) (taken current) })
                    else
                        Just (mkBufferIndex prev, Entry { fromPC = mkTag prev, taken = repeat 0b00 })
                FetchT.NoPref -> Nothing
    (_, Just upd) -> r
        where
            prev = FetchT.hFrom upd
            FetchT.GlobalHistory history = FetchT.hHistory upd
            r = case FetchT.hTaken upd of
                True ->
                    if fromPC current == mkTag prev then
                        Just (mkBufferIndex prev, current { taken = replace history (boundedAdd (taken current !! history) 1) (taken current) })
                    else
                        Just (mkBufferIndex prev, Entry { fromPC = mkTag prev, taken = repeat 0b11 })
                False ->
                    if fromPC current == mkTag prev then
                        Just (mkBufferIndex prev, current { taken = replace history (boundedSub (taken current !! history) 1) (taken current) })
                    else
                        Just (mkBufferIndex prev, Entry { fromPC = mkTag prev, taken = repeat 0b00 })
    _ -> Nothing

mkTag :: FetchT.PC -> BitVector (30 - IndexBits)
mkTag = slice d31 (SNat :: SNat (2 + IndexBits))

mkOut :: (FetchT.PC, FetchT.GlobalHistory, Entry) -> Maybe Bool
mkOut (pc, FetchT.GlobalHistory historyIndex, entry) = if fromPC entry == mkTag pc then Just (testBit (taken entry !! historyIndex) 1) else Nothing
