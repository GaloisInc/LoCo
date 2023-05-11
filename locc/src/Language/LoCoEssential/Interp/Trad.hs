{-# LANGUAGE FlexibleContexts #-}

module Language.LoCoEssential.Interp.Trad (interpret) where

import Control.Monad
import Control.Monad.Except
import Data.Map qualified as Map
import Language.LoCoEssential.Essence

interpret ::
  (MonadError String m, Eval m e v) =>
  LoCoModule e ->
  m (Env v)
interpret lMod = foldM extendEnv mempty (Map.toList (lModBinds lMod))
  where
    extendEnv env (ident, rhs) =
      case rhs of
        RHSExpr e -> evalBind env ident e
        RHSMap e s -> throwError "can't do lazy vectors yet"

evalBind ::
  Eval m e v =>
  Env v ->
  Symbol ->
  e ->
  m (Env v)
evalBind env ident e =
  do
    v <- eval env e
    pure (Map.insert ident v env)