module Orange.Types.Issue where

import Clash.Prelude
import qualified Orange.Types.Fetch as FetchT

data FunctionUnitActivation = FunctionUnitActivation {
    fuInt1 :: Maybe IssuePort,
    fuInt2 :: Maybe IssuePort,
    fuBranch :: Maybe IssuePort,
    fuMem :: Maybe IssuePort,
    fuCtrl :: Maybe IssuePort
} deriving (Generic, NFDataX)

type IssuePort = (FetchT.PC, FetchT.Inst, FetchT.Metadata)

data IssueWidth = IssueZero | IssueOne | IssueTwo
    deriving (Generic, NFDataX)
