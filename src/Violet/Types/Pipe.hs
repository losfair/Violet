module Violet.Types.Pipe where

import Clash.Prelude
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Fetch as FetchT
import qualified Violet.Types.Ctrl as CtrlT

type MemAddr = BitVector 32
type MemData = BitVector 32

data Exception = EarlyExc EarlyException
    | EarlyExcResolution (FetchT.PC, Maybe ArchRegister)
    | BranchFalsePos FetchT.PC FetchT.GlobalHistory
    | BranchFalseNeg FetchT.PC FetchT.GlobalHistory
    | BranchLink FetchT.PC GprT.RegIndex FetchT.PC
    deriving (Generic, NFDataX)
data ArchRegister = GPR GprT.RegIndex GprT.RegValue
    deriving (Generic, NFDataX)
data EarlyException = DecodeFailure FetchT.PC
    | IOMemRead FetchT.PC GprT.RegIndex MemAddr
    | IOMemWrite FetchT.PC MemAddr MemData
    deriving (Generic, NFDataX, Eq)
data Commit = Ok (FetchT.PC, Maybe ArchRegister, Maybe FetchT.HistoryUpdate) | Exc (FetchT.PC, Exception) | Bubble
    deriving (Generic, NFDataX)
data Recovery = IsRecovery | NotRecovery deriving (Generic, NFDataX, Show)

type PipeSize = 3
