{-# LANGUAGE OverloadedStrings #-}

module Language.LoCoEssential.Parse where

import Control.Monad (void)
import Data.Char (isAlphaNum, isLower, isNumber, isSpace)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Language.LoCoEssential.Essence
import Language.LoCoEssential.Expr
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
    separator = single ','

parseBind :: Parser rhs -> Parser (Symbol, rhs)
parseBind parseRhs =
  do
    s <- symbol
    void (single '=')
    e <- parseRhs
    pure (s, e)

{-
Expr -> Lit
      | Var
      | Expr '+' Expr
      | (Expr)

eliminating LR yields:

Expr -> Lit Expr'
      | Var Expr'
      | (Expr) Expr'

Expr' -> '+' Expr Expr' | epsilon
-}

expr :: Parser Expr
expr =
  choice
    [ eLit >>= expr',
      eVar >>= expr',
      parenthesized expr >>= expr'
    ]

eLit :: Parser Expr
eLit = ELit <$> integer

eVar :: Parser Expr
eVar = EVar <$> symbol

integer :: Parser Int
integer = read <$> some (satisfy isNumber)

symbol :: Parser Symbol
symbol =
  do
    c <- satisfy isLower
    cs <- many (satisfy isAlphaNum)
    pure (c : cs)

expr' :: Expr -> Parser Expr
expr' e =
  choice
    [ eAdd' e,
      pure e
    ]

eAdd' :: Expr -> Parser Expr
eAdd' e1 =
  do
    void (single '+')
    e2 <- expr
    expr' (EAdd e1 e2)

-- ws :: Parser ()
-- ws = void (many (satisfy isSpace))

parenthesized :: Parser a -> Parser a
parenthesized = between (single '(') (single ')')

braced :: Parser a -> Parser a
braced = between (single '{') (single '}')

-- between' :: Parser open -> Parser close -> Parser a -> Parser a
-- between' open close = between (open >> ws) (ws >> close)
