module Orange.Types.Pipe where

import Clash.Prelude
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Fetch as FetchT

data Exception = DecodeExc | MemoryExc
data ArchRegister = GPR GprT.RegIndex GprT.RegValue

data Commit = Ok (FetchT.PC, Maybe ArchRegister) | Exc (FetchT.PC, Exception) | Bubble
data Recovery = IsRecovery | NotRecovery
