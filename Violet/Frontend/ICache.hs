module Violet.Frontend.ICache where

import Clash.Prelude
import Violet.Types.ICache
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT

emptyResultPair :: ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata))
emptyResultPair = ((0, FetchT.nopInst, FetchT.emptyMetadata), (0, FetchT.nopInst, FetchT.emptyMetadata))

icache :: HiddenClockResetEnable dom
       => ICacheImpl a
       => a
       -> Signal dom (FetchT.PC, FetchT.Metadata)
       -> Signal dom FetchT.PC
       -> Signal dom (Maybe Bool, Maybe Bool)
       -> Signal dom FifoT.FifoPushCap
       -> Signal dom (FetchT.PreDecodeCmd, FetchT.PreDecodeAck, (FifoT.FifoItem, FifoT.FifoItem), FetchT.GlobalHistory)
icache impl fetchReq btbPrediction bhtPrediction pushCap = bundle (pdCmdReg, pdAckReg, regIssuePorts, fmap FetchT.GlobalHistory globalHistory)
    where
        (fetchPC, _) = unbundle fetchReq
        alignedPC = fmap (\x -> slice d31 d3 x ++# 0) fetchPC
        delayedFetchReq = FifoT.gatedRegister (0, FetchT.emptyMetadata) pushCap fetchReq
        (_, delayedFetchMd) = unbundle delayedFetchReq
        rawAccessRes = issueAccess impl alignedPC pushCap
        accessRes = fmap decodeAccessResult $ bundle (rawAccessRes, delayedFetchReq)

        -- Global prediction.
        globalHistory = FifoT.gatedRegister 0 pushCap (fmap f $ bundle (globalHistory, rawPredicted))
            where
                f (prev, (_, _, _, False)) = prev
                f (prev, (cmd, _, _, True)) = (shiftL prev 1) .|. v
                    where
                        v = case cmd of
                            FetchT.EarlyRectifyBranch _ -> 1
                            _ -> 0
        rawPredicted = fmap staticPredictNext $ bundle (btbPrediction, bhtPrediction, accessRes)
        (pdCmd, pdAck, resultPair, writeGlobalHistory) = unbundle $ fmap f $ bundle (rawPredicted, rectifyApply)
            where
                f (x, rectifyApply) = if rectifyApply then (FetchT.NoPreDecCmd, FetchT.NoPreDecAck, emptyResultPair, False) else x

        issuePorts = fmap f $ bundle (pushCap, resultPair, globalHistory)
            where
                f (pushCap, ((pc1, inst1, md1), (pc2, inst2, md2)), globalHistory) = case pushCap of
                    FifoT.CanPush ->
                        (
                            if FetchT.isValidInst md1 then FifoT.Item (pc1, inst1, md1 { FetchT.globalHistory = FetchT.GlobalHistory globalHistory }) else FifoT.Bubble,
                            if FetchT.isValidInst md2 then FifoT.Item (pc2, inst2, md2 { FetchT.globalHistory = FetchT.GlobalHistory globalHistory }) else FifoT.Bubble
                        )
                    _ -> (FifoT.Bubble, FifoT.Bubble)
        regIssuePorts = register (FifoT.Bubble, FifoT.Bubble) issuePorts

        -- Reduce latency for predicted branches by one cycle
        -- (pdCmdReg, pdAckReg) = unbundle $ FifoT.gatedRegister (FetchT.NoPreDecCmd, FetchT.NoPreDecAck) pushCap $ bundle (pdCmd, pdAck)
        (pdCmdReg, pdAckReg) = (pdCmd, pdAck)

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
                        (Maybe Bool, Maybe Bool),
                        ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata))
                     )
                  -> (FetchT.PreDecodeCmd, FetchT.PreDecodeAck, ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata)), Bool)
staticPredictNext (btbPrediction, (bht1, bht2), ((pc1, inst1, md1), (pc2, inst2, md2))) = (cmd, ack, (out1, out2), writeGlobalHistory)
    where
        pred1 = predictBr pc1 inst1 md1 bht1
        pred2 = predictBr pc2 inst2 md2 bht2
        jalr1 = isValidJalr inst1 md1
        jalr2 = isValidJalr inst2 md2
        (cmd, out1, out2) = case (pred1, pred2, jalr1, jalr2) of
            (Just dst, _, _, _) -> (FetchT.EarlyRectifyBranch dst, (pc1, inst1, markBranchPredicted md1 dst), (0, FetchT.nopInst, FetchT.emptyMetadata))
            (_, Just dst, _, _) -> (FetchT.EarlyRectifyBranch dst, (pc1, inst1, md1), (pc2, inst2, markBranchPredicted md2 dst))
            (_, _, True, _) -> (FetchT.EarlyRectifyBranch btbPrediction, (pc1, inst1, markBranchPredicted md1 btbPrediction), (0, FetchT.nopInst, FetchT.emptyMetadata))
            (_, _, _, True) -> (FetchT.EarlyRectifyBranch btbPrediction, (pc1, inst1, md1), (pc2, inst2, markBranchPredicted md2 btbPrediction))
            _ -> (FetchT.NoPreDecCmd, (pc1, inst1, md1), (pc2, inst2, md2))
        ack = if FetchT.exceptionResolved md1 then FetchT.AckExceptionResolved else FetchT.NoPreDecAck

        writeGlobalHistory = isCondBr inst1 || isCondBr inst2

        -- Unconditional/backwards
        isCondBr inst = slice d6 d0 inst == 0b1100011
        isCondBrBack inst = isCondBr inst && testBit inst 31
        isUncondBr inst = slice d6 d0 inst == 0b1101111
        isValidJalr inst md = FetchT.isValidInst md && slice d6 d0 inst == 0b1100111
        predictBr pc inst md bht = if FetchT.isValidInst md then next else Nothing
            where
                next = if isCondBr inst && bht == Just True then Just (pc + FetchT.decodeRelBrOffset inst)
                        else if isCondBr inst && bht == Just False then Nothing
                        else if isCondBrBack inst then Just (pc + FetchT.decodeRelBrOffset inst)
                        else if isUncondBr inst then Just (pc + FetchT.decodeJalOffset inst)
                        else Nothing

markBranchPredicted :: FetchT.Metadata -> FetchT.PC -> FetchT.Metadata
markBranchPredicted x pc = x { FetchT.branchPredicted = Just pc }