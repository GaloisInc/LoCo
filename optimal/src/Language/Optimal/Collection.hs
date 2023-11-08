{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Optimal.Collection where

import Data.Set (Set)
import Data.Set qualified as Set

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

  delete :: e -> c -> c

  {-# MINIMAL (member | notMember), insert, delete #-}

instance Ord e => Collection (Set e) e where
  member = Set.member
  insert = Set.insert
  singleton = Set.singleton
  delete = Set.delete
