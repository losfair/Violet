module VioletTest.TestICache where

import Clash.Prelude
import qualified Prelude
import Violet.Types
import qualified Violet.Memory.ICache as ICache
import qualified Violet.Memory.IBus as IBus
import qualified Violet.Memory.ICacheInterface as ICacheInterface

test :: Prelude.IO ()
test = do
    Prelude.putStrLn "OK"
