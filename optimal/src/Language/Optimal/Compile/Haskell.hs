module Language.Optimal.Compile.Haskell where

import Language.Haskell.TH (Exp, Quote)

class Haskell a where
  asExp :: Quote m => a -> m Exp

instance Haskell Exp where
  asExp = pure
