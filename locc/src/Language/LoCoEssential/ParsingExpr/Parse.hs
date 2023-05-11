{-# LANGUAGE OverloadedStrings #-}

module Language.LoCoEssential.ParsingExpr.Parse where

import Control.Monad (void)
import Data.Char (isAscii)
import Data.Text (Text)
import Data.Vector qualified as V
import Data.Void (Void)
import Language.LoCoEssential.ParsingExpr.Expr
import Language.LoCoEssential.SimpleExpr.Parse (integer, parenthesized, symbol)
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

eLit :: Parser Expr
eLit = ELit <$> value

value :: Parser Value
value =
  choice
    [ vInt,
      vString
    ]

vInt :: Parser Value
vInt = VInt <$> integer

vString :: Parser Value
vString = VString . V.fromList <$> str

str :: Parser String
str = quoted (many (satisfy (\c -> c /= '"' && isAscii c)))

-- identifier
eVar :: Parser Expr
eVar = EVar <$> symbol

-- load("<filepath>")
eLoad :: Parser Expr
eLoad =
  do
    void (string "load")
    ELoad <$> parenthesized str

-- R(<e>, <e>)
eRegion :: Parser Expr
eRegion =
  do
    void (single 'R')
    (e1, e2) <- parenthesized $
      do
        e1 <- parseExpr
        void (single ',')
        e2 <- parseExpr
        pure (e1, e2)
    pure (ERegion e1 e2)

-- parse(<ty>, <input>, <loc>)
eParse :: Parser Expr
eParse =
  do
    void (string "parse")
    (ty, input, loc) <- parenthesized $
      do
        ty <- parseParseTy
        void (single ',')
        input <- parseExpr
        void (single ',')
        loc <- parseExpr
        pure (ty, input, loc)
    pure (EParse ty input loc)

parseParseTy :: Parser ParseTy
parseParseTy =
  choice
    [ Integer <$ string "int"
    ]

quoted :: Parser a -> Parser a
quoted = between (single '"') (single '"')

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
