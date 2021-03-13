module Violet.Trace where

import Clash.Prelude
import qualified Debug.Trace as T

traceValue :: Show a
    => Show b
    => a -> b -> b
traceValue ll rr = snd $ T.traceShowId (ll, rr)
