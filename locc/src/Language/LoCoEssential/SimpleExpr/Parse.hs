{-# LANGUAGE FlexibleContexts #-}
module Language.LoCoEssential.SimpleExpr.Parse where

import Control.Monad (void)
import Data.Char (isAlphaNum, isLower, isNumber, isSpace)
import Data.Text (Text)
import Data.Void (Void)
import Language.LoCoEssential.Essence (Symbol)
import Language.LoCoEssential.SimpleExpr.Expr
import Text.Megaparsec

-- TODO: whitespace handling is unprincipled at best - best fix is probably to
-- have a lexing pass

type Parser v =
  Parsec
    Void -- error type
    Text -- input type
    v

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

parseExpr :: Parser Expr
parseExpr =
  choice
    [ eLit >>= parseExpr',
      eVar >>= parseExpr',
      parenthesized parseExpr >>= parseExpr'
    ]
    <* ws

eLit :: Parser Expr
eLit = ELit <$> integer <* ws

eVar :: Parser Expr
eVar = EVar <$> symbol <* ws

parseExpr' :: Expr -> Parser Expr
parseExpr' e =
  choice
    [ eAdd' e,
      pure e
    ]
    <* ws

eAdd' :: Expr -> Parser Expr
eAdd' e1 =
  do
    ignore (single '+')
    e2 <- parseExpr
    parseExpr' (EAdd e1 e2) <* ws

-------------------------------------------------------------------------------

integer :: MonadParsec error Text m => m Int
integer = read <$> some (satisfy isNumber) <* ws

symbol :: MonadParsec error Text m => m Symbol
symbol =
  do
    c <- satisfy isLower
    cs <- many (satisfy isAlphaNum)
    ws
    pure (c : cs)

parenthesized :: MonadParsec error Text m => m a -> m a
parenthesized p = between (ignore (single '(')) (ignore (single ')')) p <* ws

braced :: MonadParsec error Text m => m a -> m a
braced p = between (ignore (single '{')) (ignore (single '}')) p <* ws

ws :: MonadParsec error Text m => m ()
ws = void (many (satisfy isSpace))

ignore :: MonadParsec error Text m => m a -> m ()
ignore p = void p >> ws