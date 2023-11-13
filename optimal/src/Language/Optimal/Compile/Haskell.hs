module Language.Optimal.Compile.Haskell where

import Language.Haskell.TH (Exp, Q)

class Haskell a where
  asExp :: a -> Q Exp

instance Haskell Exp where
  asExp = pure