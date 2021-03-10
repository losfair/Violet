module Violet.Types.Gpr where

import Clash.Prelude
import qualified Violet.Types.Fetch as FetchT

type RegIndex = BitVector 5
type RegValue = BitVector 32

type WritePort = Maybe (RegIndex, RegValue)

decodeRs :: FetchT.Inst -> (RegIndex, RegIndex)
decodeRs inst = (slice d19 d15 inst, slice d24 d20 inst)

decodeRd :: FetchT.Inst -> RegIndex
decodeRd = slice d11 d7
