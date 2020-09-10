module Orange.Types.DecodeDep where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT

data Activation = Activation {
    actInt :: Bool,
    actBranch :: Bool,
    actMem :: Bool,
    actCtrl :: Bool,
    actException :: Bool
} deriving (Generic, NFDataX)

data Concurrency = CanConcurrentIssue | NoConcurrentIssue
    deriving (Generic, NFDataX)

data RegLayout = Rd | RdRs1 | RdRs1Rs2 | Rs1Rs2 | NoReg
    deriving (Generic, NFDataX, Eq)

data Stall = StallNone | StallMemory | StallControl
    deriving (Generic, NFDataX)

type DecodeDepBundle = (FetchT.PC, FetchT.Inst, Activation, RegLayout, Concurrency, Stall)
