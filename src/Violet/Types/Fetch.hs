module Violet.Types.Fetch where

import Clash.Prelude
import qualified Violet.Config as Config
import qualified Violet.TypeLevel.Nat

type PC = BitVector 32
type Inst = BitVector 32

type GlobalHistoryBits = $(Violet.TypeLevel.Nat.natT Config.gshareGlobalHistoryBits)

data Metadata = Metadata {
    branchPredicted :: Maybe PC,
    exceptionResolved :: Bool,
    earlyRectifyApplied :: Bool,
    globalHistory :: GlobalHistory,
    icMiss :: Bool,
    isValidInst :: Bool
} deriving (Generic, NFDataX, Show)
data BackendCmd = NoCmd | ApplyBranch (PC, (PC, PredictionPref))
    deriving (Generic, NFDataX, Show)
data PreDecodeCmd = NoPreDecCmd | EarlyRectifyBranch PC
    deriving (Generic, NFDataX, Show)
data PreDecodeAck = AckExceptionResolved | NoPreDecAck
    deriving (Generic, NFDataX, Show)
data PredictionPref = Taken GlobalHistory | NotTaken GlobalHistory | NoPref deriving (Generic, NFDataX, Show)
data HistoryUpdate = HistoryUpdate {
    hFrom :: PC,
    hTaken :: Bool,
    hHistory :: GlobalHistory
} deriving (Generic, NFDataX, Show)
data GlobalHistory = GlobalHistory (BitVector GlobalHistoryBits)
    deriving (Generic, NFDataX, Show)

emptyMetadata :: Metadata
emptyMetadata = Metadata { branchPredicted = Nothing, exceptionResolved = False, earlyRectifyApplied = False, globalHistory = GlobalHistory 0, icMiss = False, isValidInst = False }

validMetadata :: Metadata
validMetadata = Metadata { branchPredicted = Nothing, exceptionResolved = False, earlyRectifyApplied = False, globalHistory = GlobalHistory 0, icMiss = False, isValidInst = True }

nopInst :: Inst
nopInst = 0b0010011 -- addi x0, x0, 0

decodeRelBrOffset :: Inst -> BitVector 32
decodeRelBrOffset inst = signExtend (slice d31 d31 inst ++# slice d7 d7 inst ++# slice d30 d25 inst ++# slice d11 d8 inst ++# (0b0 :: BitVector 1))

decodeJalOffset :: Inst -> BitVector 32
decodeJalOffset inst = signExtend (slice d31 d31 inst ++# slice d19 d12 inst ++# slice d20 d20 inst ++# slice d30 d21 inst ++# (0b0 :: BitVector 1))