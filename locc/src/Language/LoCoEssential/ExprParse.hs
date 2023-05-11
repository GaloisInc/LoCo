{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.LoCoEssential.ExprParse where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Language.LoCoEssential.Essence

data Expr
  = ELit Value
  | EVar Symbol
  | ELoad FilePath
  | EAdd Expr Expr
  | ERegion {eRegionBegin :: Expr, eRegionEnd :: Expr}
  | EParse {eParseTy :: ParseTy, eParseInput :: Expr, eParseLoc :: Expr}

data ParseTy
  = Integer

data Value
  = VInt Int
  | VRegion Int Int
  | VString (Vector Char)
  deriving (Show)

instance FreeVars Expr where
  fvs e =
    case e of
      ELit _ -> mempty
      EVar v -> Set.singleton v
      ELoad _ -> mempty
      EAdd e1 e2 -> fvs e1 <> fvs e2
      ERegion begin end -> fvs begin <> fvs end
      EParse _ str loc -> fvs str <> fvs loc

evaluate :: (MonadIO m, MonadError IOError m) => Env Value -> Expr -> m Value
evaluate env = go
  where
    go expr =
      case expr of
        ELit v -> pure v
        EVar s ->
          case env Map.!? s of
            Nothing -> err "variable not found"
            Just v -> pure v
        ELoad f -> VString . V.fromList <$> liftIO (readFile f)
        EAdd e1 e2 ->
          do
            v1 <- go e1
            v2 <- go e2
            case (v1, v2) of
              (VInt i1, VInt i2) -> pure (VInt (i1 + i2))
              _ -> err "type error"
        ERegion e1 e2 ->
          do
            v1 <- go e1
            v2 <- go e2
            case (v1, v2) of
              (VInt i1, VInt i2) -> pure (VRegion i1 i2)
              _ -> err "type error"
        EParse parseTy str loc ->
          do
            strVal <- go str
            locVal <- go loc
            case (strVal, locVal) of
              (VString input, VRegion begin end) ->
                either err pure (parseWith parseTy (V.slice begin end input))
              _ -> err "type error"

    err = throwError . userError

parseWith :: ParseTy -> Vector Char -> Either String Value
parseWith parseTy v =
  case parseTy of
    Integer -> pure (VInt (read (V.toList v)))

instance Eval IO Expr Value where
  eval = evaluate
