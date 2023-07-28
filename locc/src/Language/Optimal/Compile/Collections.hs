{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Optimal.Compile.Collections where

import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts (IsList (..))
import Language.Haskell.TH (Name)

class Monoid c => Collection c e | c -> e where
  member :: e -> c -> Bool
  default member :: e -> c -> Bool
  member e c = not (notMember e c)

  notMember :: e -> c -> Bool
  default notMember :: e -> c -> Bool
  notMember e c = not (member e c)

  insert :: e -> c -> c

  singleton :: e -> c
  default singleton :: e -> c
  singleton e = insert e mempty

  {-# MINIMAL (member | notMember), insert #-}


