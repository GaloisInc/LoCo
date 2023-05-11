{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.LoCoEssential.Expr where

import Control.Monad.Except (throwError)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Language.LoCoEssential.Essence

data Expr
  = ELit Int
  | EVar Symbol
  | EAdd Expr Expr
  deriving (Show)

evaluate :: Env Int -> Expr -> Either String Int
evaluate env expr =
  case expr of
    ELit i -> pure i
    EVar s ->
      case env Map.!? s of
        Just v -> pure v
        Nothing -> throwError "variable not found"
    EAdd e1 e2 ->
      do
        v1 <- evaluate env e1
        v2 <- evaluate env e2
        pure (v1 + v2)

instance FreeVars Expr where
  fvs e =
    case e of
      ELit _ -> mempty
      EVar v -> Set.singleton v
      EAdd e1 e2 -> fvs e1 <> fvs e2

instance Eval IO Expr Int where
  eval env expr =
    case evaluate env expr of
      Left err -> throwError $ userError err
      Right v -> pure v

-- compileStrict :: LoCoModule Expr -> [Ident] -> Map Ident TH.Exp
-- compileStrict = undefined
