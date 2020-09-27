module Violet.Types.Issue where

import Clash.Prelude
import qualified Violet.Types.Fetch as FetchT

data FunctionUnitActivation = FunctionUnitActivation {
    fuInt1 :: Maybe IssuePort,
    fuInt2 :: Maybe IssuePort,
    fuBranch :: Maybe IssuePort,
    fuMem :: Maybe IssuePort,
    fuCtrl :: Maybe (IssuePort, ControlIssue)
} deriving (Generic, NFDataX)

type IssuePort = (FetchT.PC, FetchT.Inst, FetchT.Metadata)

data ControlIssue = CtrlNormal | CtrlDecodeException | CtrlFetchException
    deriving (Generic, NFDataX)

data ActivationMask = ActivationMask {
    amInt1 :: Bool,
    amInt2 :: Bool,
    amBranch :: Bool,
    amMem :: Bool,
    amCtrl :: Maybe ControlIssue
} deriving (Generic, NFDataX)
