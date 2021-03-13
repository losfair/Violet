module Violet.IP.DirectMappedICache where

import Clash.Prelude
import Violet.Types.ICache

import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Ctrl as CtrlT
import qualified Violet.Trace as T

-- bits 31:(SizeBits + 3) -> Tag
-- bits (SizeBits + 2):3 -> Data Index
-- bits (SizeBits + 2):(LineBits + 3) -> Tag Index
-- bits (LineBits + 2):0 -> Line
-- bits 2:0 -> Word
type SizeBits = 12
type LineBits = 2
type TagBits = (32 - (SizeBits + 3))

data TagEntry = Tagged (BitVector TagBits) | Untagged
    deriving (Generic, NFDataX, Eq, Show)

issueAccess :: HiddenClockResetEnable dom
    => Signal dom CtrlT.IcRefillIn
    -> ICacheIssueAccess dom
issueAccess refillIn pc_ pushCap = dataValue
    where
        enable = (hideEnable genEnable) pushCap
        pc = T.traceValue "DirectMappedICache.pc" <$> pc_
        rdExpectedTag = fmap (unpack . getTagValue) pc
        rdDataIndex = fmap (unpack . getDataIndex) pc
        rdTagIndex = fmap (unpack . getTagIndex) pc
        rdActualTag = T.traceValue "DirectMappedICache.actualTagRead" <$> blockRamPow2 (repeat Untagged) rdTagIndex tagWr
        lowerDataPort = T.traceValue "DirectMappedICache.lowerDataPort" <$> blockRamU NoClearOnReset (SNat :: SNat (2^SizeBits)) undefined rdDataIndex lowerDataWr
        upperDataPort = T.traceValue "DirectMappedICache.upperDataPort" <$> blockRamU NoClearOnReset (SNat :: SNat (2^SizeBits)) undefined rdDataIndex upperDataWr

        tagHit = T.traceValue "DirectMappedICache.tagHit" <$> (\(a, b) -> a == b) <$> T.traceValue "DirectMappedICache.tagHit.input" <$> bundle (Tagged <$> rdExpectedTag, rdActualTag)
        firstCycle = register True $ pure False
        dataValue = T.traceValue "DirectMappedICache.dataValue" <$> gate <$> bundle (firstCycle, tagHit, upperDataPort, lowerDataPort)
            where
                gate (firstCycle, tagHit, upperDataPort, lowerDataPort) =
                    if not firstCycle && tagHit then
                        Just (upperDataPort ++# lowerDataPort)
                    else
                        Nothing

        -- Input bus is 32-bit so we need to interleave writes
        dataWrOdd = extract <$> refillIn
            where
                extract refillIn = testBit (CtrlT.iIcRefillAddr refillIn) 2
        dataWrEven = not <$> dataWrOdd

        lowerDataWr = T.traceValue "DirectMappedICache.lowerDataWr" <$> transformDataWr <$> bundle (refillIn, dataWrEven)
        upperDataWr = T.traceValue "DirectMappedICache.upperDataWr" <$> transformDataWr <$> bundle (refillIn, dataWrOdd)
        tagWr = transformTagWr <$> refillIn

        transformDataWr :: (CtrlT.IcRefillIn, Bool) -> Maybe (Unsigned SizeBits, BitVector 32)
        transformDataWr (refillIn, en) =
            if CtrlT.iIcRefillValid refillIn && en then
                Just (unpack $ getDataIndex $ CtrlT.iIcRefillAddr refillIn, CtrlT.iIcRefillData refillIn)
            else
                Nothing
        transformTagWr :: CtrlT.IcRefillIn -> Maybe (Unsigned (SizeBits - LineBits), TagEntry)
        transformTagWr refillIn =
            if CtrlT.iIcRefillValid refillIn then
                Just (unpack $ getTagIndex $ CtrlT.iIcRefillAddr refillIn, Tagged $ getTagValue $ CtrlT.iIcRefillAddr refillIn)
            else
                Nothing

        getTagValue :: BitVector 32 -> BitVector TagBits
        getTagValue = slice d31 (SNat :: SNat (SizeBits + 3))

        getDataIndex :: BitVector 32 -> BitVector SizeBits
        getDataIndex = slice (SNat :: SNat (SizeBits + 2)) d3

        getTagIndex :: BitVector 32 -> BitVector (SizeBits - LineBits)
        getTagIndex = slice (SNat :: SNat (SizeBits + 2)) (SNat :: SNat (LineBits + 3))

        genEnable :: HiddenClockResetEnable dom => Enable dom -> Signal dom FifoT.FifoPushCap -> Enable dom
        genEnable base x = toEnable (fmap (\(x, y) -> x && y) $ bundle (rawBase, canPush))
            where
                rawBase = fromEnable base
                canPush = fmap (\x -> case x of
                    FifoT.CanPush -> True
                    _ -> False
                    ) x
                    