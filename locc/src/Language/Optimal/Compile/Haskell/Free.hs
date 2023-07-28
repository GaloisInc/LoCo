{-# LANGUAGE DefaultSignatures #-}

module Language.Optimal.Compile.Haskell.Free where

import Data.Set (Set)
import Language.Haskell.TH (Exp, Name)
import Language.Optimal.Compile.Collections
import Language.Optimal.Compile.Haskell.RenameFree (RenameFree (..))

class Free a where
  freeVars :: a -> Set Name
  default freeVars :: RenameFree a => a -> Set Name
  freeVars a =
    let (_, FreeVars frees) = renameFree mempty id a
     in frees

instance Free Exp
