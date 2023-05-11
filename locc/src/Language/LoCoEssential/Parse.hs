{-# LANGUAGE OverloadedStrings #-}

module Language.LoCoEssential.Parse where

import Control.Monad (void)
import Data.Char (isSpace)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Language.LoCoEssential.Essence
import Language.LoCoEssential.SimpleExpr.Parse (braced, symbol, ws)
import Text.Megaparsec

type Parser v =
  Parsec
    Void -- error type
    Text -- input type
    v

{-
abcModule =
  { a = 3
  , b = a + a
  }
 -}

abcModule :: Text
abcModule = sanitize "abcModule = { a = 3, b = a + a }"

sanitize :: Text -> Text
sanitize = Text.filter (not . isSpace)

parseModule :: Parser expr -> Parser (LoCoModule expr)
parseModule parseExpr =
  do
    (s, m) <- parseBind (parseModuleBindings (RHSExpr <$> parseExpr))
    pure (LoCoModule s m)

parseModuleBindings :: Parser expr -> Parser (Env expr)
parseModuleBindings parseExpr =
  do
    binds <- braced (parseBind parseExpr `sepBy1` separator)
    pure (Map.fromList binds)
  where
    separator = single ',' >> ws

parseBind :: Parser rhs -> Parser (Symbol, rhs)
parseBind parseRhs =
  do
    s <- symbol
    ws
    void (single '=')
    ws
    e <- parseRhs
    pure (s, e)
