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
       -> Signal dom FetchT.PC
       -> Signal dom FifoT.FifoPushCap
       -> Signal dom (FetchT.PreDecodeCmd, FetchT.PreDecodeAck, ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata)))
icache impl fetchReq btbPrediction pushCap = bundle (pdCmdReg, pdAckReg, regAccessRes)
    where
        (fetchPC, _) = unbundle fetchReq
        alignedPC = fmap (\x -> slice d31 d3 x ++# 0) fetchPC
        delayedFetchReq = FifoT.gatedRegister (0, FetchT.emptyMetadata) pushCap fetchReq
        (_, delayedFetchMd) = unbundle delayedFetchReq
        rawAccessRes = issueAccess impl alignedPC pushCap
        accessRes = fmap decodeAccessResult $ bundle (rawAccessRes, delayedFetchReq)
        rawPredicted = fmap staticPredictNext $ bundle (btbPrediction, accessRes)
        (pdCmd, pdAck, afterPrediction) = unbundle $ fmap f $ bundle (rawPredicted, rectifyApply)
            where
                f (x, rectifyApply) = if rectifyApply then (FetchT.NoPreDecCmd, FetchT.NoPreDecAck, emptyResultPair) else x
        regAccessRes = FifoT.gatedRegister emptyResultPair pushCap afterPrediction
        (pdCmdReg, pdAckReg) = unbundle $ FifoT.gatedRegister (FetchT.NoPreDecCmd, FetchT.NoPreDecAck) pushCap $ bundle (pdCmd, pdAck)

        rectifyApplyBuf = FifoT.gatedRegister False pushCap $ fmap f $ bundle (rectifyApplyBuf, delayedFetchMd, pdCmd)
            where
                f (current, md, cmd) = case (cmd, FetchT.earlyRectifyApplied md) of
                    (FetchT.EarlyRectifyBranch _, _) -> True
                    (_, True) -> False
                    _ -> current
        rectifyApply = fmap f $ bundle (rectifyApplyBuf, delayedFetchMd)
            where
                f (buf, md) = if FetchT.earlyRectifyApplied md then False else buf

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
            False -> (setBit pc 2, slice d63 d32 rawRes, if FetchT.isValidInst fetchMeta then FetchT.validMetadata else FetchT.emptyMetadata)

staticPredictNext :: (
                        FetchT.PC,
                        ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata))
                     )
                  -> (FetchT.PreDecodeCmd, FetchT.PreDecodeAck, ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata)))
staticPredictNext (btbPrediction, ((pc1, inst1, md1), (pc2, inst2, md2))) = (cmd, ack, (out1, out2))
    where
        pred1 = predictBr pc1 inst1 md1
        pred2 = predictBr pc2 inst2 md2
        jalr1 = isValidJalr inst1 md1
        jalr2 = isValidJalr inst2 md2
        (cmd, out1, out2) = case (pred1, pred2, jalr1, jalr2) of
            (Just dst, _, _, _) -> (FetchT.EarlyRectifyBranch dst, (pc1, inst1, markBranchPredicted md1 dst), (0, FetchT.nopInst, FetchT.emptyMetadata))
            (_, Just dst, _, _) -> (FetchT.EarlyRectifyBranch dst, (pc1, inst1, md1), (pc2, inst2, markBranchPredicted md2 dst))
            (_, _, True, _) -> (FetchT.EarlyRectifyBranch btbPrediction, (pc1, inst1, markBranchPredicted md1 btbPrediction), (0, FetchT.nopInst, FetchT.emptyMetadata))
            (_, _, _, True) -> (FetchT.EarlyRectifyBranch btbPrediction, (pc1, inst1, md1), (pc2, inst2, markBranchPredicted md2 btbPrediction))
            _ -> (FetchT.NoPreDecCmd, (pc1, inst1, md1), (pc2, inst2, md2))
        ack = if FetchT.exceptionResolved md1 then FetchT.AckExceptionResolved else FetchT.NoPreDecAck

        -- Unconditional/backwards
        isCondBrBack inst = slice d6 d0 inst == 0b1100011 && testBit inst 31
        isUncondBr inst = slice d6 d0 inst == 0b1101111
        isValidJalr inst md = FetchT.isValidInst md && slice d6 d0 inst == 0b1100111
        predictBr pc inst md = if FetchT.isValidInst md then next else Nothing
            where
                next = if isCondBrBack inst then Just (pc + FetchT.decodeRelBrOffset inst)
                        else if isUncondBr inst then Just (pc + FetchT.decodeJalOffset inst)
                        else Nothing

markBranchPredicted :: FetchT.Metadata -> FetchT.PC -> FetchT.Metadata
markBranchPredicted x pc = x { FetchT.branchPredicted = Just pc }