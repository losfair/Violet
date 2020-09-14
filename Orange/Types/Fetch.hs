module Orange.Types.Fetch where

import Clash.Prelude

type PC = BitVector 32
type Inst = BitVector 32
data Metadata = Metadata {
    branchPredicted :: Bool,
    exceptionResolved :: Bool,
    isValidInst :: Bool
} deriving (Generic, NFDataX, Show)
data BackendCmd = NoCmd | ApplyBranch (PC, (PC, PredictionPref)) deriving (Generic, NFDataX, Show)
data PredictionPref = Taken | NotTaken | Unconditional deriving (Generic, NFDataX, Show)

emptyMetadata :: Metadata
emptyMetadata = Metadata { branchPredicted = False, exceptionResolved = False, isValidInst = False }

validMetadata :: Metadata
validMetadata = Metadata { branchPredicted = False, exceptionResolved = False, isValidInst = True }

nopInst :: Inst
nopInst = 0b0010011 -- addi x0, x0, 0

decodeRelBrOffset :: Inst -> BitVector 32
decodeRelBrOffset inst = signExtend (slice d31 d31 inst ++# slice d7 d7 inst ++# slice d30 d25 inst ++# slice d11 d8 inst ++# (0b0 :: BitVector 1))

decodeJalOffset :: Inst -> BitVector 32
decodeJalOffset inst = signExtend (slice d31 d31 inst ++# slice d19 d12 inst ++# slice d20 d20 inst ++# slice d30 d21 inst ++# (0b0 :: BitVector 1))