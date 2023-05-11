module Language.LoCoEssential.SimpleExpr.Parse where

import Control.Monad (void)
import Data.Char (isAlphaNum, isLower, isNumber)
import Data.Text (Text)
import Data.Void (Void)
import Language.LoCoEssential.Essence (Symbol)
import Language.LoCoEssential.SimpleExpr.Expr
import Text.Megaparsec

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

eLit :: Parser Expr
eLit = ELit <$> integer

eVar :: Parser Expr
eVar = EVar <$> symbol

parseExpr' :: Expr -> Parser Expr
parseExpr' e =
  choice
    [ eAdd' e,
      pure e
    ]

eAdd' :: Expr -> Parser Expr
eAdd' e1 =
  do
    void (single '+')
    e2 <- parseExpr
    parseExpr' (EAdd e1 e2)

-------------------------------------------------------------------------------

integer :: Parser Int
integer = read <$> some (satisfy isNumber)

symbol :: Parser Symbol
symbol =
  do
    c <- satisfy isLower
    cs <- many (satisfy isAlphaNum)
    pure (c : cs)

parenthesized :: Parser a -> Parser a
parenthesized = between (single '(') (single ')')

braced :: Parser a -> Parser a
braced = between (single '{') (single '}')