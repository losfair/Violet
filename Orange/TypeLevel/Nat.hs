{-# LANGUAGE TemplateHaskell #-}
module Orange.TypeLevel.Nat where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Prelude

natT :: Integer -> TypeQ
natT n = pure (LitT (NumTyLit n))
