{-# LANGUAGE DefaultSignatures #-}

module Language.Optimal.Compile.Haskell.Rename where

import Language.Haskell.TH (Exp, Name)
import Language.Optimal.Compile.Haskell.RenameFree (RenameFree (..))

class Rename a where
  rename :: (Name -> Name) -> a -> a
  default rename :: RenameFree a => (Name -> Name) -> a -> a
  rename f x =
    let (x', _) = renameFree mempty f x
     in x'

instance Rename Exp
