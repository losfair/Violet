module OrangeTest.TestICache where

import Clash.Prelude
import qualified Prelude
import Orange.Types
import qualified Orange.Memory.ICache as ICache
import qualified Orange.Memory.IBus as IBus
import qualified Orange.Memory.ICacheInterface as ICacheInterface

test :: Prelude.IO ()
test = do
    Prelude.putStrLn "OK"
