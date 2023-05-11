{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.LoCoEssential.Interp.Lazy where

import Control.Monad (foldM)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Data.Map qualified as Map
import Language.LoCoEssential.Essence
import Language.LoCoEssential.Expr
import Language.LoCoEssential.Thunk

interpret' ::
  (MonadIO m, MonadError IOError m, Eval m Expr Int) =>
  LoCoModule Expr ->
  m (Env (Thunk m Int))
interpret' lMod = foldM extendEnv mempty (Map.toList (lModBinds lMod))
  where
    extendEnv env (ident, rhs) =
      case rhs of
        RHSExpr e -> evalBindLazy env ident e
        RHSMap e s -> throwError $ userError "can't do lazy vectors yet"

evalBindLazy ::
  (MonadIO m, Eval m Expr Int) =>
  Env (Thunk m Int) ->
  Symbol ->
  Expr ->
  m (Env (Thunk m Int))
evalBindLazy env ident e =
  do
    vThunk <- delay $
      do
        let env' = Map.restrictKeys env (fvs e)
        liftIO $ putStrLn $ "forcing " <> ident
        env'' <- mapM force env'
        eval env'' e

    pure (Map.insert ident vThunk env)

interpret ::
  (MonadIO m, MonadError IOError m, Eval m Expr Int) =>
  LoCoModule Expr ->
  m (Env (m Int))
interpret lmod =
  do
    env <- interpret' lmod
    pure $ fmap force env

smallModule :: LoCoModule Expr
smallModule =
  LoCoModule "abc" $
    Map.fromList
      [ ("a", RHSExpr (ELit 1)),
        ("b", RHSExpr (EAdd (EVar "a") (EVar "a"))),
        ("c", RHSExpr (error "c"))
      ]

testB :: IO Int
testB =
  do
    m <- interpret smallModule
    b <- m Map.! "b"
    a <- m Map.! "a"
    pure (b + a)
