{-# LANGUAGE FlexibleContexts #-}

module Language.LoCoEssential.Interp.Trad (interpret) where

import Control.Monad
import Control.Monad.Except
import Data.Map qualified as Map
import Language.LoCoEssential.Essence

interpret ::
  (MonadError IOError m, Eval m e v, FreeVars e) =>
  LoCoModule e ->
  m (Env v)
interpret lMod =
  case orderedBinds lMod of
    Nothing -> throwError $ userError "cycle"
    Just env -> foldM extendEnv mempty env
  where
    extendEnv env (ident, rhs) =
      case rhs of
        RHSExpr e -> evalBind env ident e
        RHSMap e s -> throwError $ userError "can't do lazy vectors yet"

evalBind ::
  (MonadError IOError m, Eval m e v) =>
  Env v ->
  Symbol ->
  e ->
  m (Env v)
evalBind env ident e =
  do
    v <- eval env e
    pure (Map.insert ident v env)