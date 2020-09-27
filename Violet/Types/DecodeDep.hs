module Violet.Types.DecodeDep where

import Clash.Prelude
import qualified Violet.Types.Fetch as FetchT

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

data RegLayout = RegLayout {
    hasRd :: Bool,
    hasRs1 :: Bool,
    hasRs2 :: Bool
} deriving (Generic, NFDataX, Eq, Show)

layoutRd = RegLayout { hasRd = True, hasRs1 = False, hasRs2 = False }
layoutRdRs1 = RegLayout { hasRd = True, hasRs1 = True, hasRs2 = False }
layoutRdRs1Rs2 = RegLayout { hasRd = True, hasRs1 = True, hasRs2 = True }
layoutRs1Rs2 = RegLayout { hasRd = False, hasRs1 = True, hasRs2 = True }
layoutNoReg = RegLayout { hasRd = False, hasRs1 = False, hasRs2 = False }

data Stall = StallNone | StallMemory | StallControl
    deriving (Generic, NFDataX, Eq, Show)

type DecodeDepBundle = (FetchT.PC, FetchT.Inst, FetchT.Metadata, Activation, RegLayout, Concurrency, Stall)
