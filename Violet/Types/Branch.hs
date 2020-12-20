module Violet.Types.Branch where

import Clash.Prelude

data SfbPredicate = SfbDisable | SfbEnable
    deriving (Generic, NFDataX, Show, Eq)
