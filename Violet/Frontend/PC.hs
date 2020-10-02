module Violet.Frontend.PC where

import Clash.Prelude
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Fifo as FifoT
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Config as Config

data DeferredBranchApplication = Deferred | NotDeferred
    deriving (Generic, NFDataX)

pc :: HiddenClockResetEnable dom
   => Signal dom FetchT.BackendCmd
   -> Signal dom (FetchT.PreDecodeCmd, FetchT.PreDecodeAck)
   -> Signal dom FifoT.FifoPushCap
   -> Signal dom (FetchT.PC, FetchT.Metadata)
pc beCmd pdGroup pushCap = out
    where
        (pdCmd_, pdAck) = unbundle pdGroup
        pdValid = isValidPredecodeFeedback $ bundle (pushCap, beCmd, pdAck)
        pdCmd = fmap maskPdCmd $ bundle (pdValid, pdCmd_)

        out = fmap (\(a, b, c) -> (a, b)) (if Config.mergePCIF1 then immCurrent else current)
        immCurrent = fmap nextPC $ bundle (pushCap, beCmd, pdCmd, current)
        current = register (if Config.mergePCIF1 then 0xfffffff8 else 0, FetchT.validMetadata, NotDeferred) immCurrent
        nextPC (pushCap, beCmd, pdCmd, current) = case (pushCap, beCmd, pdCmd, current) of
            (FifoT.WillFull, FetchT.ApplyBranch (new, _), _, _) -> (new, if Config.mergePCIF1 then FetchT.validMetadata else mkBranchAppliedMetadata, Deferred)
            (FifoT.CanPush, FetchT.ApplyBranch (new, _), _, _) -> (new, mkBranchAppliedMetadata, NotDeferred)
            (FifoT.CanPush, _, FetchT.EarlyRectifyBranch new, _) -> (new, mkBranchEarlyRectifiedMetadata, NotDeferred)
            (FifoT.CanPush, _, _, (currentPC, _, Deferred)) | Config.mergePCIF1 -> (currentPC, mkBranchAppliedMetadata, NotDeferred)
            (FifoT.CanPush, _, _, (currentPC, _, _)) -> (addPC currentPC, FetchT.validMetadata, NotDeferred)
            (_, _, _, (currentPC, currentMd, deferBr)) -> (currentPC, currentMd, deferBr)

        maskPdCmd (valid, cmd) = case valid of
            True -> cmd
            False -> FetchT.NoPreDecCmd


addPC :: FetchT.PC -> FetchT.PC
addPC x = (slice d31 d3 x + 1) ++# 0

-- An applied branch from backend implies early rectification resolution.
mkBranchAppliedMetadata :: FetchT.Metadata
mkBranchAppliedMetadata = FetchT.validMetadata { FetchT.exceptionResolved = True, FetchT.earlyRectifyApplied = True }

mkBranchEarlyRectifiedMetadata :: FetchT.Metadata
mkBranchEarlyRectifiedMetadata = FetchT.validMetadata { FetchT.earlyRectifyApplied = True }

-- Early rectifications between backend branch request and ack should be ignored.
-- And responses from predecode are only valid when the gated registers are enabled (FifoT.CanPush).
isValidPredecodeFeedback' :: Bool
                          -> (FifoT.FifoPushCap, FetchT.BackendCmd, FetchT.PreDecodeAck)
                          -> (Bool, Bool)
isValidPredecodeFeedback' _ (FifoT.CanPush, _, FetchT.AckExceptionResolved) = (True, True)
isValidPredecodeFeedback' _ (_, FetchT.ApplyBranch _, _) = (False, False)
isValidPredecodeFeedback' x _ = (x, x)

isValidPredecodeFeedback :: HiddenClockResetEnable dom
                         => Signal dom (FifoT.FifoPushCap, FetchT.BackendCmd, FetchT.PreDecodeAck)
                         -> Signal dom Bool
isValidPredecodeFeedback = mealy isValidPredecodeFeedback' True
