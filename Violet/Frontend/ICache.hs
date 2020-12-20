module Violet.Frontend.ICache where

import Clash.Prelude
import Clash.Explicit.SimIO
import Violet.Types.ICache
import qualified Violet.Config as Config
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.TypeLevel.Nat

type SfbMaxLengthBits = $(Violet.TypeLevel.Nat.natT Config.sfbMaxLengthBits)
type SfbMaxLength = 2 ^ SfbMaxLengthBits
type SfbCounter = BitVector SfbMaxLengthBits

emptyResultPair :: ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata))
emptyResultPair = ((0, FetchT.nopInst, FetchT.emptyMetadata), (0, FetchT.nopInst, FetchT.emptyMetadata))

icache :: HiddenClockResetEnable dom
       => ICacheImpl a
       => a
       -> Signal dom (FetchT.PC, FetchT.Metadata)
       -> Signal dom FetchT.PC
       -> Signal dom (Maybe Bool, Maybe Bool)
       -> Signal dom FifoT.FifoPushCap
       -> Signal dom (FetchT.PreDecodeCmd, FetchT.PreDecodeAck, (FifoT.FifoItem, FifoT.FifoItem), FetchT.GlobalHistory, (FetchT.PC, SfbCounter))
icache impl fetchReq btbPrediction bhtPrediction pushCap = bundle (pdCmdReg, pdAckReg, regIssuePorts, fmap FetchT.GlobalHistory globalHistory, bundle (debugPC, sfbCounter))
    where
        (fetchPC, _) = unbundle fetchReq
        alignedPC = fmap (\x -> slice d31 d3 x ++# 0) fetchPC
        delayedFetchReq = FifoT.gatedRegister (0, FetchT.emptyMetadata) pushCap fetchReq
        (_, delayedFetchMd) = unbundle delayedFetchReq
        rawAccessRes = issueAccess impl alignedPC pushCap
        accessRes = fmap decodeAccessResult $ bundle (rawAccessRes, delayedFetchReq)
        sfbCounter = if Config.sfbElimination then regEn 0 ((==) FifoT.CanPush <$> pushCap) nextSfbCounter else pure undefined

        -- Global prediction.
        globalHistory = FifoT.gatedRegister 0 pushCap (fmap f $ bundle (globalHistory, rawPredicted))
            where
                f (prev, (_, _, _, False)) = prev
                f (prev, (cmd, _, _, True)) = (shiftL prev 1) .|. v
                    where
                        v = case cmd of
                            FetchT.EarlyRectifyBranch _ -> 1
                            _ -> 0
        (rawPredicted, nextSfbCounter) = unbundle $ fmap staticPredictNext $ bundle (btbPrediction, bhtPrediction, accessRes, sfbCounter)
        debugPC = fmap (\((pc1, _, _), _) -> pc1) accessRes
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
                        ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata)),
                        SfbCounter
                     )
                  -> (
                      (FetchT.PreDecodeCmd, FetchT.PreDecodeAck, ((FetchT.PC, FetchT.Inst, FetchT.Metadata), (FetchT.PC, FetchT.Inst, FetchT.Metadata)), Bool),
                      SfbCounter
                      )
staticPredictNext (btbPrediction, (bht1, bht2), ((pc1, inst1, md1), (pc2, inst2, md2)), sfbCounter') = ((cmd, ack, (out1, out2), writeGlobalHistory), nextSfbCounter)
    where
        -- When a branch happens, reset sfb counter
        sfbCounter =
            if (FetchT.earlyRectifyApplied md1 || FetchT.exceptionResolved md1) then
                0
            else
                sfbCounter'

        sfbEnable1 = if Config.sfbElimination then FetchT.isValidInst md1 && sfbCounter == 0 && isSfb inst1 else False
        sfbEnable2 = if Config.sfbElimination then FetchT.isValidInst md2 && sfbCounter <= 1 && isSfb inst2 && not sfbEnable1 else False
        markAsCondExec1 = if Config.sfbElimination then FetchT.isValidInst md1 && sfbCounter > 0 else False
        markAsCondExec2 = if Config.sfbElimination then FetchT.isValidInst md2 && (sfbCounter > 1 || sfbEnable1) else False

        nextSfbCounter = if not Config.sfbElimination then undefined else
            if sfbEnable1 then
                if FetchT.isValidInst md2 then
                    sfbCounterInit inst1 - 1
                else
                    sfbCounterInit inst1
            else if sfbEnable2 then
                sfbCounterInit inst2
            else if sfbCounter == 1 then
                if FetchT.isValidInst md1 || FetchT.isValidInst md2 then
                    sfbCounter - 1
                else
                    sfbCounter
            else if sfbCounter == 0 then
                0
            else
                if FetchT.isValidInst md1 && FetchT.isValidInst md2 then
                    sfbCounter - 2
                else if FetchT.isValidInst md1 || FetchT.isValidInst md2 then
                    sfbCounter - 1
                else
                    sfbCounter

        pred1 = if markAsCondExec1 then Nothing else predictBr pc1 inst1 md1 bht1
        pred2 = if markAsCondExec2 then Nothing else predictBr pc2 inst2 md2 bht2
        jalr1 = isValidJalr inst1 md1 && not markAsCondExec1
        jalr2 = isValidJalr inst2 md2 && not markAsCondExec2
        (cmd, out1'', out2'') = case (pred1, pred2, jalr1, jalr2) of
            (Just dst, _, _, _) -> (FetchT.EarlyRectifyBranch dst, (pc1, inst1, markBranchPredicted md1 dst), (0, FetchT.nopInst, FetchT.emptyMetadata))
            (_, Just dst, _, _) -> (FetchT.EarlyRectifyBranch dst, (pc1, inst1, md1), (pc2, inst2, markBranchPredicted md2 dst))
            (_, _, True, _) -> (FetchT.EarlyRectifyBranch btbPrediction, (pc1, inst1, markBranchPredicted md1 btbPrediction), (0, FetchT.nopInst, FetchT.emptyMetadata))
            (_, _, _, True) -> (FetchT.EarlyRectifyBranch btbPrediction, (pc1, inst1, md1), (pc2, inst2, markBranchPredicted md2 btbPrediction))
            _ -> (FetchT.NoPreDecCmd, (pc1, inst1, md1), (pc2, inst2, md2))
        ack = if FetchT.exceptionResolved md1 then FetchT.AckExceptionResolved else FetchT.NoPreDecAck
        out1' = if markAsCondExec1 then addCondExecMark out1'' else out1''
        out2' = if markAsCondExec2 then addCondExecMark out2'' else out2''
        out1 = if sfbEnable1 then addSfbMark out1' else out1'
        out2 = if sfbEnable2 then addSfbMark out2' else out2'

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
        isSfb inst = isCondBr inst && not (isCondBrBack inst) && FetchT.decodeRelBrOffsetShort inst >= 8 && FetchT.decodeRelBrOffsetShort inst <= (snatToNum (SNat :: SNat SfbMaxLength)) * 4
        sfbCounterInit inst = (slice (SNat :: SNat (1 + SfbMaxLengthBits)) d2 $ FetchT.decodeRelBrOffsetShort inst) - 1
        addCondExecMark (pc, inst, md) = (pc, inst, md { FetchT.isConditional = True })
        addSfbMark (pc, inst, md) = (pc, inst, md { FetchT.isSfb = True })

markBranchPredicted :: FetchT.Metadata -> FetchT.PC -> FetchT.Metadata
markBranchPredicted x pc = x { FetchT.branchPredicted = Just pc }