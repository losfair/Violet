module Orange.Backend.Gpr where

import Clash.Prelude
import Orange.Types.Gpr
import qualified Orange.Types.Fifo as FifoT

type RegFile = Vec 32 RegValue

gpr' :: (RegFile, ((RegValue, RegValue), (RegValue, RegValue)))
     -> ((FifoT.FifoItem, FifoT.FifoItem), WritePort, WritePort)
     -> ((RegFile, ((RegValue, RegValue), (RegValue, RegValue))), ((RegValue, RegValue), (RegValue, RegValue)))
gpr' (rf, results) ((item1, item2), wp1, wp2) = ((rf', results'), results)
    where
        rf_ = case wp1 of
            Just (i, v) -> replace i v rf
            Nothing -> rf
        rf' = case wp2 of
            Just (i, v) -> replace i v rf_
            Nothing -> rf_
        (r1, r2) = fifoItemToRegIndices item1
        (r3, r4) = fifoItemToRegIndices item2
        results' = ((regFetch rf' r1, regFetch rf' r2), (regFetch rf' r3, regFetch rf' r4))

gpr :: HiddenClockResetEnable dom
    => Signal dom ((FifoT.FifoItem, FifoT.FifoItem), WritePort, WritePort)
    -> Signal dom ((RegValue, RegValue), (RegValue, RegValue))
gpr = mealy gpr' (repeat 0, ((0, 0), (0, 0)))

fifoItemToRegIndices :: FifoT.FifoItem -> (RegIndex, RegIndex)
fifoItemToRegIndices item = (rs1, rs2)
    where
        inst = case item of
            FifoT.Item (_, inst, _, _, _, _, _) -> inst
            _ -> 0
        rs1 = slice d19 d15 inst
        rs2 = slice d24 d20 inst

regFetch :: RegFile -> RegIndex -> RegValue
regFetch rf 0 = 0
regFetch rf i = rf !! i
