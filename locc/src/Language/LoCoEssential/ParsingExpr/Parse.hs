{-# LANGUAGE OverloadedStrings #-}

module Language.LoCoEssential.ParsingExpr.Parse where

import Control.Applicative (Applicative (liftA2))
import Control.Monad (void)
import Data.Char (isAscii)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Void (Void)
import Language.LoCoEssential.ParsingExpr.Expr
import Language.LoCoEssential.SimpleExpr.Parse (ignore, integer, parenthesized, symbol, ws)
import Text.Megaparsec
import Text.Megaparsec.Char (string)

{-
parseABC =
  { src = load("sample.txt")
  , r = R(0,4)
  , v = parse(int,src,r)
  }
-}

type Parser v =
  Parsec
    Void -- error type
    Text -- input type
    v

{-
Expr -> Lit
      | Var
      | 'load' string
      | Expr '+' Expr
      | 'R' '(' Expr ',' Expr ')'
      | 'parse' ParseTy Expr Expr
      | '(' Expr ')'

Lit -> Value

Value -> int
       | R '(' int ',' int ')'
       | string

Var -> identifier

strings are enclosed in quotes
identifiers start with a lowercase character

ParseTy -> 'u8'

-->

Expr -> Lit Expr'
      | Var Expr'
      | 'load' string Expr'
      | 'R' Expr Expr Expr'
      | 'parse' ParseTy Expr Expr Expr'

Expr' -> '+' Expr
       | epsilon
 -}

parseExpr :: Parser Expr
parseExpr =
  choice
    [ eLit >>= parseExpr',
      eLoad >>= parseExpr',
      eRegion >>= parseExpr',
      eParse >>= parseExpr',
      eVar >>= parseExpr',
      parenthesized parseExpr >>= parseExpr'
    ]
    <* ws

eLit :: Parser Expr
eLit = ELit <$> value <* ws

value :: Parser Value
value =
  choice
    [ vInt,
      vString
    ]
    <* ws

vInt :: Parser Value
vInt = VInt <$> integer <* ws

vString :: Parser Value
vString = VString . V.fromList <$> str <* ws

str :: Parser String
str = quoted (many (satisfy nonQuote)) <* ws
  where
    nonQuote = liftA2 (&&) (/= '"') isAscii

-- identifier
eVar :: Parser Expr
eVar = EVar <$> symbol <* ws

-- load("<filepath>")
eLoad :: Parser Expr
eLoad =
  do
    ignore (string "load")
    ELoad <$> parenthesized str <* ws

-- R(<e>, <e>)
eRegion :: Parser Expr
eRegion =
  do
    ignore (single 'R')
    (e1, e2) <- parenthesized $
      do
        e1 <- parseExpr
        void (single ',')
        e2 <- parseExpr
        pure (e1, e2)
    ws
    pure (ERegion e1 e2)

-- parse(<ty>, <input>, <loc>)
eParse :: Parser Expr
eParse =
  do
    ignore (string "parse")
    (ty, input, loc) <- parenthesized $
      do
        ty <- parseParseTy
        ignore (single ',')
        input <- parseExpr
        ignore (single ',')
        loc <- parseExpr
        pure (ty, input, loc)
    ws
    pure (EParse ty input loc)

parseParseTy :: Parser ParseTy
parseParseTy =
  choice
    [ Integer <$ string "int"
    ]
    <* ws

quoted :: Parser a -> Parser a
quoted p = between (ignore (single '"')) (ignore (single '"')) p <* ws

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
