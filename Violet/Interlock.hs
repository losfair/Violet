module Violet.Interlock where

import Clash.Prelude

data InterlockState = Locked | Unlocked
    deriving (Generic, NFDataX, Eq)

class (Generic a, NFDataX a) => Interlockable a where
    isInterlocked :: a -> InterlockState

data Interlock a = Interlock InterlockState a
    deriving (Generic, NFDataX)

instance Interlockable a => Interlockable (Interlock a) where
    isInterlocked (Interlock Locked _) = Locked
    isInterlocked (Interlock Unlocked x) = isInterlocked x

data NullInterlock = NullInterlock
    deriving (Generic, NFDataX)

instance Interlockable NullInterlock where
    isInterlocked NullInterlock = Unlocked
