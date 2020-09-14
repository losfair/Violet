module Orange.Frontend.ICache where

import Clash.Prelude
import Orange.Types.ICache
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Fifo as FifoT

emptyResultPair :: ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata))
emptyResultPair = ((0, FetchT.nopInst, FetchT.emptyMetadata), (0, FetchT.nopInst, FetchT.emptyMetadata))

icache :: HiddenClockResetEnable dom
       => ICacheImpl a
       => a
       -> Signal dom (FetchT.PC, FetchT.Metadata)
       -> Signal dom FifoT.FifoPushCap
       -> Signal dom ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata))
icache impl fetchReq pushCap = regAccessRes
    where
        (fetchPC, _) = unbundle fetchReq
        alignedPC = fmap (\x -> slice d31 d3 x ++# 0) fetchPC
        delayedFetchReq = FifoT.gatedRegister (0, FetchT.emptyMetadata) pushCap fetchReq
        rawAccessRes = issueAccess impl alignedPC pushCap
        accessRes = fmap decodeAccessResult $ bundle (rawAccessRes, delayedFetchReq)
        regAccessRes = FifoT.gatedRegister emptyResultPair pushCap accessRes

decodeAccessResult :: (Maybe (BitVector 64), (FetchT.PC, FetchT.Metadata))
                   -> ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata))
decodeAccessResult (Nothing, _) = emptyResultPair
decodeAccessResult (Just rawRes, (pc, fetchMeta)) = ((pc1, inst1, md1), (pc2, inst2, md2))
    where
        hasOffset = testBit pc 2
        (pc1, inst1, md1) = case hasOffset of
            True -> (pc, slice d63 d32 rawRes, fetchMeta)
            False -> (pc, slice d31 d0 rawRes, fetchMeta)
        (pc2, inst2, md2) = case hasOffset of
            True -> (0, FetchT.nopInst, FetchT.emptyMetadata)
            False -> (setBit pc 2, slice d63 d32 rawRes, FetchT.validMetadata)