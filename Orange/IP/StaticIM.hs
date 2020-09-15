module Orange.IP.StaticIM where

import Clash.Prelude
import Orange.Types.ICache

import qualified Orange.Types.Fetch as FetchT
import qualified Orange.Types.Fifo as FifoT

data StaticIM = StaticIM

instance ICacheImpl StaticIM where
    issueAccess _ pc pushCap = fmap Just readPort
        where
            enable = (hideEnable genEnable) pushCap
            readAddr = fmap (unpack . slice d15 d3) pc
            readPort = (exposeEnable (blockRamFilePow2 "im.bin" readAddr (pure Nothing))) enable
            genEnable :: HiddenClockResetEnable dom => Enable dom -> Signal dom FifoT.FifoPushCap -> Enable dom
            genEnable base x = toEnable (fmap (\(x, y) -> x && y) $ bundle (rawBase, canPush))
                where
                    rawBase = fromEnable base
                    canPush = fmap (\x -> case x of
                        FifoT.CanPush -> True
                        _ -> False
                        ) x
                        