{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.LoCoEssential.Interp.Lazy (interpret) where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map qualified as Map
import Language.LoCoEssential.Essence
import Language.LoCoEssential.Thunk

interpret ::
  (MonadIO m, MonadError IOError m, FreeVars e, Eval m e v) =>
  LoCoModule e ->
  m (Env (m v))
interpret lMod =
  do
    env <- interpret' lMod
    pure $ fmap force env

interpret' ::
  (MonadIO m, FreeVars e, MonadError IOError m, Eval m e v) =>
  LoCoModule e ->
  m (Env (Thunk m v))
interpret' lMod = 
  case orderedBinds lMod of
    Nothing -> throwError $ userError "cycle"
    Just env -> foldM extendEnv mempty env
  where
    extendEnv env (ident, rhs) =
      case rhs of
        RHSExpr e -> evalBindLazy env ident e
        RHSMap e s -> throwError $ userError "can't do lazy vectors yet"

evalBindLazy ::
  (MonadIO m, FreeVars e, Eval m e v) =>
  Env (Thunk m v) ->
  Symbol ->
  e ->
  m (Env (Thunk m v))
evalBindLazy env ident e =
  do
    vThunk <- delay $
      do
        let env' = Map.restrictKeys env (fvs e)
        liftIO $ putStrLn $ "forcing " <> ident
        env'' <- mapM force env'
        eval env'' e

    pure (Map.insert ident vThunk env)
