module Violet.Memory.ICacheTypes where

import Prelude
import Violet.TypeLevel.Nat
import qualified Violet.Config as Config

-- | tag | entry_index | line_internal_address
type Entries = $(natT (Config.instructionCacheEntries))
type EntryIndexBits = $(natT (floor $ logBase 2 $ fromIntegral Config.instructionCacheEntries))
type LineInternalAddressBits = $(natT (floor $ logBase 2 $ fromIntegral Config.instructionCacheLineWidth))
type TagBits = 32 - EntryIndexBits - LineInternalAddressBits
