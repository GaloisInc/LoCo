{-# LANGUAGE FunctionalDependencies #-}

module Language.LoCoEssential.Essence where

import Data.Map (Map)
import Data.Set (Set)

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

class FreeVars e where
  fvs :: e -> Set Symbol

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
