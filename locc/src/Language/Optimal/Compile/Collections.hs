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

-------------------------------------------------------------------------------

newtype FreeVars = FreeVars (Set Name)
  deriving (Eq, Show)

newtype BindingVars = BindingVars (Set Name)
  deriving (Eq, Show)

instance Semigroup FreeVars where
  FreeVars f1 <> FreeVars f2 = FreeVars (f1 <> f2)

instance Semigroup BindingVars where
  BindingVars f1 <> BindingVars f2 = BindingVars (f1 <> f2)

instance Monoid FreeVars where
  mempty = FreeVars mempty

instance Monoid BindingVars where
  mempty = BindingVars mempty

instance Collection FreeVars Name where
  member e (FreeVars fvs) = Set.member e fvs
  insert e (FreeVars fvs) = FreeVars (Set.insert e fvs)

instance Collection BindingVars Name where
  member e (BindingVars bvs) = Set.member e bvs
  insert e (BindingVars bvs) = BindingVars (Set.insert e bvs)

instance IsList FreeVars where
  type Item FreeVars = Name
  fromList names = FreeVars (Set.fromList names)
  toList (FreeVars fvs) = Set.toList fvs

instance IsList BindingVars where
  type Item BindingVars = Name
  fromList names = BindingVars (Set.fromList names)
  toList (BindingVars bvs) = Set.toList bvs
