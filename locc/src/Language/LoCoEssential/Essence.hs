{-# LANGUAGE FunctionalDependencies #-}

module Language.LoCoEssential.Essence where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Language.LoCo.Toposort (topoSortPossibly)

type Symbol = String

type Env a = Map Symbol a

-- `e`: the expression language for computation
data RHS e
  = -- | Just a computation
    RHSExpr e
  | -- | Introduction form for lazy vector. The introduction form for
    -- strict vectors is encapsulated in `e`.
    RHSMap e Symbol
  deriving (Show)

data LoCoModule e = LoCoModule {lModName :: Symbol, lModBinds :: Env (RHS e)}
  deriving (Show)

orderedBinds :: (FreeVars e) => LoCoModule e -> Maybe [(Symbol, RHS e)]
orderedBinds lMod =
  case topoSortPossibly (Map.toList (Set.toList . fvs <$> lModBinds lMod)) of
    Nothing -> Nothing
    Just vs -> Just $ reverse [(v, lModBinds lMod Map.! v) | v <- vs]

class FreeVars e where
  fvs :: e -> Set Symbol

instance FreeVars e => FreeVars (RHS e) where
  fvs rhs =
    case rhs of
      RHSExpr e -> fvs e
      RHSMap e _ -> fvs e

class Monad m => Eval m expr value | expr -> value where
  eval :: Env value -> expr -> m value

class Eval m expr value => InterpStrict m expr value where -- fundep: expr -> value?
  interpStrict :: LoCoModule expr -> m (Env value)

class Eval m expr value => InterpLazy m expr value where -- fundep: expr -> value?
  interpLazy :: LoCoModule expr {- -> Runtime m e v -} -> m (Env (m value))

-- data Runtime m e v = forall thunkID.
--   Runtime
--   { rtDelay :: m e -> m thunkID,
--     rtForce :: thunkID -> m v
--   }
