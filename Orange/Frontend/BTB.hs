module Orange.Frontend.BTB where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Fifo as FifoT
import qualified Orange.Types.Gpr as GprT

type Entry = Maybe (BitVector 31)
type BufferBits = 8 :: Nat

btb :: HiddenClockResetEnable dom
    => Signal dom FetchT.BackendCmd
    -> Signal dom FetchT.PreDecodeCmd
    -> Signal dom FetchT.PC
    -> Signal dom (Maybe FetchT.PC)
btb beCmd pdCmd prevPC = fmap (fmap (\x -> x ++# (0 :: BitVector 1))) readData
    where 
        writeData = fmap mkBufferUpdate $ bundle (beCmd, pdCmd)
        readData = asyncRamPow2 (fmap mkIndex prevPC) writeData

mkBufferUpdate :: (FetchT.BackendCmd, FetchT.PreDecodeCmd) -> Maybe (Unsigned BufferBits, Entry)
mkBufferUpdate (beCmd, pdCmd) = case (beCmd, pdCmd) of
    (FetchT.ApplyBranch (nextPC, (prevPC, pref)), _) -> let prevI = mkIndex prevPC in
        case pref of
            FetchT.Taken -> Just (prevI, mkEntry nextPC)
            FetchT.NotTaken -> Just (prevI, Nothing)
            FetchT.Unconditional -> Just (prevI, mkEntry nextPC)
    (_, FetchT.EarlyRectifyBranch (nextPC, prevPC)) -> let prevI = mkIndex prevPC in
        case nextPC of
            Just nextPC -> Just (prevI, mkEntry nextPC)
            Nothing -> Just (prevI, Nothing)
    _ -> Nothing

mkIndex :: FetchT.PC -> Unsigned BufferBits
mkIndex = unpack . slice (SNat :: SNat (1 + BufferBits)) d2

mkEntry :: FetchT.PC -> Entry
mkEntry = Just . slice d31 d1
