module Language.Optimal.Compile.Haskell.TH where

import Language.Haskell.TH (Exp)

class Haskell a where
  asExp :: a -> Either String Exp

instance Haskell Exp where
  asExp = pure
