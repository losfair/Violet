module Orange.Types.Pipe where

import Clash.Prelude
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Fetch as FetchT

data Exception = DecodeExc | MemoryExc | BranchFalsePos FetchT.PC | BranchFalseNeg FetchT.PC | BranchLink FetchT.PC GprT.RegIndex FetchT.PC
    deriving (Generic, NFDataX)
data ArchRegister = GPR GprT.RegIndex GprT.RegValue
    deriving (Generic, NFDataX)

data Commit = Ok (FetchT.PC, Maybe ArchRegister) | Exc (FetchT.PC, Exception) | Bubble
    deriving (Generic, NFDataX)
data Recovery = IsRecovery | NotRecovery deriving (Generic, NFDataX, Show)

type PipeSize = 3 :: Nat
