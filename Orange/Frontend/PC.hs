module Orange.Frontend.PC where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Fifo as FifoT
import qualified Orange.Types.Gpr as GprT

pc :: HiddenClockResetEnable dom
   => Signal dom FetchT.BackendCmd
   -> Signal dom FifoT.FifoPushCap
   -> Signal dom (FetchT.PC, FetchT.Metadata)
pc cmd pushCap = current
    where
        current = register (0x0, FetchT.validMetadata) (fmap nextPC $ bundle (pushCap, cmd, current))
        nextPC (pushCap, cmd, current) = case (pushCap, cmd, current) of
            (_, FetchT.ApplyBranch (new, _), _) -> (new, mkBranchAppliedMetadata)
            (FifoT.CanPush, _, (currentPC, _)) -> (addPC currentPC, FetchT.validMetadata)
            (_, _, (currentPC, currentMd)) -> (currentPC, currentMd)

addPC :: FetchT.PC -> FetchT.PC
addPC x = (slice d31 d3 x + 1) ++# 0

mkBranchAppliedMetadata :: FetchT.Metadata
mkBranchAppliedMetadata = FetchT.validMetadata { FetchT.exceptionResolved = True }
