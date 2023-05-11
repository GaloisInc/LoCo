{-# LANGUAGE FlexibleContexts #-}

module Language.LoCoEssential.Interp.Trad where

import Control.Monad
import Control.Monad.Except
import Data.Map qualified as Map
import Language.LoCoEssential.Essence
import Language.LoCoEssential.Expr

interpret ::
  (MonadError String m, Eval m Expr Int) =>
  LoCoModule Expr ->
  m (Env Int)
interpret lMod = foldM extendEnv mempty (Map.toList (lModBinds lMod))
  where
    extendEnv env (ident, rhs) =
      case rhs of
        RHSExpr e -> evalBind env ident e
        RHSMap e s -> throwError "can't do lazy vectors yet"

evalBind ::
  Eval m Expr Int =>
  Env Int ->
  Symbol ->
  Expr ->
  m (Env Int)
evalBind env ident e =
  do
    v <- eval env e
    pure (Map.insert ident v env)