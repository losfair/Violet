module Violet.Types.Commit where

import Clash.Prelude
import qualified Violet.Types.Gpr as GprT
import qualified Violet.Types.Fetch as FetchT

data CommitLog = CommitLog {
    pc1 :: Maybe FetchT.PC,
    writePort1 :: GprT.WritePort,

    pc2 :: Maybe FetchT.PC,
    writePort2 :: GprT.WritePort
} deriving (Generic, NFDataX, Show)

emptyCommitLog :: CommitLog
emptyCommitLog = CommitLog { pc1 = Nothing, writePort1 = Nothing, pc2 = Nothing, writePort2 = Nothing }
