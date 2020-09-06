module Orange.Config where

import Clash.Prelude

instructionCacheSize = 4096
instructionCacheLineWidth = 32

instructionCacheEntries = instructionCacheSize `div` instructionCacheLineWidth
