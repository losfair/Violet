module Orange.Types.Fetch where

import Clash.Prelude

type PC = BitVector 32
type Inst = BitVector 32
data Metadata = Metadata {
    branchPredicted :: Bool,
    exceptionResolved :: Bool
} deriving (Generic, NFDataX)

emptyMetadata :: Metadata
emptyMetadata = Metadata { branchPredicted = False, exceptionResolved = False }

nopInst :: Inst
nopInst = 0b0010011 -- addi x0, x0, 0
