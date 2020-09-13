module Orange.Types.Fetch where

import Clash.Prelude

type PC = BitVector 32
type Inst = BitVector 32
data Metadata = Metadata {
    branchPredicted :: Bool,
    exceptionResolved :: Bool,
    isValidInst :: Bool
} deriving (Generic, NFDataX, Show)
data BackendCmd = NoCmd | ApplyBranch (PC, Maybe (PC, PredictionPref)) deriving (Generic, NFDataX, Show)
data PredictionPref = Taken | NotTaken deriving (Generic, NFDataX, Show)

emptyMetadata :: Metadata
emptyMetadata = Metadata { branchPredicted = False, exceptionResolved = False, isValidInst = False }

nopInst :: Inst
nopInst = 0b0010011 -- addi x0, x0, 0
