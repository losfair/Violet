module Violet.Types.Issue where

import Clash.Prelude
import qualified Violet.Types.Fetch as FetchT

data FunctionUnitActivation = FunctionUnitActivation {
    fuInt1 :: Maybe IssuePort,
    fuInt2 :: Maybe IssuePort,
    fuBranch1 :: Maybe IssuePort,
    fuBranch2 :: Maybe IssuePort,
    fuLateInt1 :: Maybe IssuePort,
    fuLateInt2 :: Maybe IssuePort,
    fuLateBranch1 :: Maybe IssuePort,
    fuLateBranch2 :: Maybe IssuePort,
    fuMem1 :: Maybe IssuePort,
    fuMem2 :: Maybe IssuePort,
    fuCtrl :: Maybe (IssuePort, ControlIssue)
} deriving (Generic, NFDataX)

type IssuePort = (FetchT.PC, FetchT.Inst, FetchT.Metadata)

data ControlIssue = CtrlNormal | CtrlDecodeException | CtrlFetchException
    deriving (Generic, NFDataX)

data ActivationMask = ActivationMask {
    amInt1 :: Bool,
    amInt2 :: Bool,
    amBranch1 :: Bool,
    amBranch2 :: Bool,
    amLateInt1 :: Bool,
    amLateInt2 :: Bool,
    amLateBranch1 :: Bool,
    amLateBranch2 :: Bool,
    amMem1 :: Bool,
    amMem2 :: Bool,
    amCtrl :: Maybe ControlIssue
} deriving (Generic, NFDataX)
