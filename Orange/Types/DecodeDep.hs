module Orange.Types.DecodeDep where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT

data Activation = Activation {
    actInt :: Bool,
    actBranch :: Bool,
    actLoad :: Bool,
    actStore :: Bool,
    actCtrl :: Bool,
    actException :: Bool
} deriving (Generic, NFDataX, Show)

data Concurrency = CanConcurrentIssue | NoConcurrentIssue
    deriving (Generic, NFDataX, Eq, Show)

data RegLayout = Rd | RdRs1 | RdRs1Rs2 | Rs1Rs2 | NoReg
    deriving (Generic, NFDataX, Eq, Show)

data Stall = StallNone | StallMemory | StallControl
    deriving (Generic, NFDataX, Eq, Show)

type DecodeDepBundle = (FetchT.PC, FetchT.Inst, FetchT.Metadata, Activation, RegLayout, Concurrency, Stall)
