module Orange.Frontend.PC where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Fifo as FifoT
import qualified Orange.Types.Gpr as GprT

pc :: HiddenClockResetEnable dom
   => Signal dom FetchT.BackendCmd
   -> Signal dom (FetchT.PreDecodeCmd, FetchT.PreDecodeAck)
   -> Signal dom FifoT.FifoPushCap
   -> Signal dom (FetchT.PC, FetchT.Metadata)
pc beCmd pdGroup pushCap = current
    where
        (pdCmd_, pdAck) = unbundle pdGroup
        pdValid = isValidPredecodeFeedback $ bundle (beCmd, pdAck)
        pdCmd = fmap maskPdCmd $ bundle (pdValid, pdCmd_)

        current = register (0x0, FetchT.validMetadata) (fmap nextPC $ bundle (pushCap, beCmd, pdCmd, current))
        nextPC (pushCap, beCmd, pdCmd, current) = case (pushCap, beCmd, pdCmd, current) of
            (_, FetchT.ApplyBranch (new, _), _, _) -> (new, mkBranchAppliedMetadata)
            (_, _, FetchT.EarlyRectifyBranch (new, prev), _) -> (resolveRectifyTarget new prev, mkBranchEarlyRectifiedMetadata)
            (FifoT.CanPush, _, _, (currentPC, _)) -> (addPC currentPC, FetchT.validMetadata)
            (_, _, _, (currentPC, currentMd)) -> (currentPC, currentMd)

        maskPdCmd (valid, cmd) = case valid of
            True -> cmd
            False -> FetchT.NoPreDecCmd

        resolveRectifyTarget new prev = case new of
            Just x -> x
            Nothing -> prev + 4


addPC :: FetchT.PC -> FetchT.PC
addPC x = (slice d31 d3 x + 1) ++# 0

mkBranchAppliedMetadata :: FetchT.Metadata
mkBranchAppliedMetadata = FetchT.validMetadata { FetchT.exceptionResolved = True }

mkBranchEarlyRectifiedMetadata :: FetchT.Metadata
mkBranchEarlyRectifiedMetadata = FetchT.validMetadata { FetchT.earlyRectifyApplied = True }

isValidPredecodeFeedback' :: Bool
                          -> (FetchT.BackendCmd, FetchT.PreDecodeAck)
                          -> (Bool, Bool)
isValidPredecodeFeedback' _ (_, FetchT.AckExceptionResolved) = (True, True)
isValidPredecodeFeedback' _ (FetchT.ApplyBranch _, _) = (False, False)
isValidPredecodeFeedback' x _ = (x, x)

isValidPredecodeFeedback :: HiddenClockResetEnable dom
                         => Signal dom (FetchT.BackendCmd, FetchT.PreDecodeAck)
                         -> Signal dom Bool
isValidPredecodeFeedback = mealy isValidPredecodeFeedback' True
