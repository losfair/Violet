module Violet.Backend.IntAlu where

import Clash.Prelude
import qualified Violet.Config as Config
import qualified Violet.Types.Pipe as PipeT
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Issue as IssueT

intAlu' :: Maybe IssueT.IssuePort
        -> (GprT.RegValue, GprT.RegValue)
        -> PipeT.Commit
intAlu' Nothing _ = PipeT.Bubble
intAlu' (Just (pc, inst, _)) (rs1V, rs2V) = PipeT.Ok (pc, Just $ PipeT.GPR rd out, Nothing)
    where
        rd = GprT.decodeRd inst
        arithFunct3 = slice d14 d12 inst

        regSrc2 = testBit inst 5
        arithSrc1 = rs1V
        arithSrc2 = if regSrc2 then rs2V else signExtend (slice d31 d20 inst)
        arithSub = testBit inst 30 && testBit inst 5
        arithSra = testBit inst 30

        arithOut = case arithFunct3 of
            0b000 -> if arithSub then arithSrc1 - arithSrc2 else arithSrc1 + arithSrc2 -- add/sub
            0b010 -> extendBoolToReg (signedLt arithSrc1 arithSrc2) -- slt
            0b011 -> extendBoolToReg (unsignedLt arithSrc1 arithSrc2) -- sltu
            0b100 -> xor arithSrc1 arithSrc2 -- xor
            0b110 -> arithSrc1 .|. arithSrc2 -- or
            0b111 -> arithSrc1 .&. arithSrc2 -- and
            0b001 -> genShiftL arithSrc1 arithSrc2 -- sll
            0b101 ->  if arithSra then signedShiftR arithSrc1 arithSrc2 else unsignedShiftR arithSrc1 arithSrc2 -- srl/sra

        mulOut =
            if Config.mulInAlu then
                case arithFunct3 of
                    0b000 -> rs1V * rs2V
                    _ -> undefined
            else undefined
        miscIn = (slice d31 d12 inst ++# 0) :: GprT.RegValue
        miscOut = case slice d5 d5 inst of
            0b1 -> miscIn -- lui
            0b0 -> miscIn + pc -- auipc
        
        out = case slice d2 d2 inst of
            0b0 -> if slice d25 d25 inst == 0b1 && regSrc2 then mulOut else arithOut
            0b1 -> miscOut

intAlu :: HiddenClockResetEnable dom
       => Signal dom (Maybe IssueT.IssuePort)
       -> Signal dom (GprT.RegValue, GprT.RegValue)
       -> Signal dom PipeT.Commit
intAlu a b = fmap (\(a, b) -> intAlu' a b) $ bundle (a, b)

extendBoolToReg :: Bool -> GprT.RegValue
extendBoolToReg False = 0
extendBoolToReg True = 1

unsignedLt :: GprT.RegValue -> GprT.RegValue -> Bool
unsignedLt a b = (unpack a :: Unsigned 32) < (unpack b :: Unsigned 32)

signedLt :: GprT.RegValue -> GprT.RegValue -> Bool
signedLt a b = (unpack a :: Signed 32) < (unpack b :: Signed 32)

genShiftL :: GprT.RegValue -> GprT.RegValue -> GprT.RegValue
genShiftL a b = shiftL a (shiftAmount b)

unsignedShiftR :: GprT.RegValue -> GprT.RegValue -> GprT.RegValue
unsignedShiftR a b = pack $ shiftR (unpack a :: Unsigned 32) (shiftAmount b)

signedShiftR :: GprT.RegValue -> GprT.RegValue -> GprT.RegValue
signedShiftR a b = pack $ shiftR (unpack a :: Signed 32) (shiftAmount b)

shiftAmount :: GprT.RegValue -> Int
shiftAmount x = fromIntegral (slice d4 d0 x)