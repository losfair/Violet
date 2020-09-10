module Orange.Types.Fetch where

import Clash.Prelude

type PC = BitVector 32
type Inst = BitVector 32

nopInst :: Inst
nopInst = 0b0010011 -- addi x0, x0, 0
