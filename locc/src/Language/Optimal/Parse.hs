{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Optimal.Parse where

import Control.Monad (void, when)
import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Data.List.NonEmpty (NonEmpty, fromList)
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH (Exp)
import Language.LoCoEssential.Essence ()
import Language.LoCoEssential.SimpleExpr.Parse (braced, ignore, ws)
import Language.Optimal.Syntax
import Text.Megaparsec

type Parser v =
  Parsec
    Void -- error type
    Text -- input type
    v

fromText :: Parser e -> Text -> Either String e
fromText parser text =
  case runParser (ws >> parser) "<stdin>" text of
    Left errBundle -> Left (errorBundlePretty errBundle)
    Right m -> pure m

-------------------------------------------------------------------------------

parseOptimalModule :: Parser OptimalModule
parseOptimalModule =
  do
    (name, ty) <- parseBinop parseVarName (single ':') parseTyName
    (name', env) <- parseBinop (chunk name) (single '=') (parseExprBindings parseHSExpr)
    pure (OptimalModule {pmTy = ty, pmEnv = NamedEnv name env})

parseOptimalTypeDecl :: Parser OptimalTypeDecl
parseOptimalTypeDecl =
  do
    ignore (chunk "type")
    (name, env) <- parseBinop parseTyName (single '=') (parseTypeBindings parseOptimalType)
    pure (OptimalTypeDecl (NamedEnv name env))

parseOptimalType :: Parser OptimalType
parseOptimalType =
  choice
    [ Bool <$ chunk "Bool",
      Char <$ chunk "Char",
      Ctor <$> parseTyName
    ]

-------------------------------------------------------------------------------

parseHSExpr :: Parser Exp
parseHSExpr =
  do
    ignore (chunk left)
    str <- manyTill anySingle (chunk right)
    case parseExp str of
      Left err -> error "parse error"
      Right expr -> pure expr
  where
    (left, right) = ("<|", "|>")

-------------------------------------------------------------------------------

parseExprBindings :: Parser expr -> Parser (Env expr)
parseExprBindings = parseBindings (single '=')

parseTypeBindings :: Parser ty -> Parser (Env ty)
parseTypeBindings = parseBindings (single ':')

-- | Parse a curly-braced, comma-separated, non-empty set of "bindings",
-- producing a mapping from symbols to expressions. Parsing is parametric over
-- binding syntax ('=', e.g.) and expression syntax (e.g. `Exp`)
parseBindings :: Parser op -> Parser rhs -> Parser (Env rhs)
parseBindings parseOp parseRhs =
  do
    binds <- braced (binding `sepBy1` separator)
    pure (Map.fromList binds)
  where
    binding = parseBinop parseVarName parseOp parseRhs
    separator = single ',' >> ws

parseBinop :: Parser lhs -> Parser op -> Parser rhs -> Parser (lhs, rhs)
parseBinop parseLhs parseOp parseRhs =
  do
    lhs <- parseLhs
    ws
    ignore parseOp
    rhs <- parseRhs
    ws
    pure (lhs, rhs)

-------------------------------------------------------------------------------

parseVarName :: MonadParsec error Text m => m Symbol
parseVarName =
  do
    c <- satisfy isLower
    cs <- many (satisfy isAlphaNum)
    ws
    pure (Text.pack (c : cs))

parseTyName :: MonadParsec error Text m => m Symbol
parseTyName =
  do
    c <- satisfy isUpper
    cs <- many (satisfy isAlphaNum)
    ws
    pure (Text.pack (c : cs))