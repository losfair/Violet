module Orange.Types.Pipe where

import Clash.Prelude
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Fetch as FetchT

data Exception = DecodeExc | MemoryExc | BranchMiss
    deriving (Generic, NFDataX)
data ArchRegister = GPR GprT.RegIndex GprT.RegValue deriving (Generic, NFDataX)

data Commit = Ok (FetchT.PC, Maybe ArchRegister) | Exc (FetchT.PC, Exception) | Bubble deriving (Generic, NFDataX)
data Recovery = IsRecovery | NotRecovery deriving (Generic, NFDataX)

type PipeSize = 3 :: Nat
