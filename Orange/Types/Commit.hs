module Orange.Types.Commit where

import Clash.Prelude
import qualified Orange.Types.Gpr as GprT
import qualified Orange.Types.Fetch as FetchT

data CommitLog = CommitLog {
    pc1 :: Maybe FetchT.PC,
    writePort1 :: GprT.WritePort,

    pc2 :: Maybe FetchT.PC,
    writePort2 :: GprT.WritePort
} deriving (Generic, NFDataX)

emptyCommitLog :: CommitLog
emptyCommitLog = CommitLog { pc1 = Nothing, writePort1 = Nothing, pc2 = Nothing, writePort2 = Nothing }
