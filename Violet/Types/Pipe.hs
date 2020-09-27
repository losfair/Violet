module Violet.Types.Pipe where

import Clash.Prelude
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Ctrl as CtrlT

data Exception = EarlyExc EarlyException
    | EarlyExcResolution (FetchT.PC, FetchT.PC, Maybe ArchRegister)
    | BranchFalsePos FetchT.PC
    | BranchFalseNeg FetchT.PC
    | BranchLink FetchT.PC GprT.RegIndex FetchT.PC
    deriving (Generic, NFDataX)
data ArchRegister = GPR GprT.RegIndex GprT.RegValue
    deriving (Generic, NFDataX)
data EarlyException = DecodeFailure FetchT.PC | CsrReadWrite FetchT.PC GprT.RegIndex CtrlT.CsrIndex GprT.RegValue
    deriving (Generic, NFDataX, Eq)
data Commit = Ok (FetchT.PC, Maybe ArchRegister) | Exc (FetchT.PC, Exception) | Bubble
    deriving (Generic, NFDataX)
data Recovery = IsRecovery | NotRecovery deriving (Generic, NFDataX, Show)

type PipeSize = 3 :: Nat
