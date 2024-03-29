{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.LoCoEssential.Samples where

import Data.Map qualified as Map
import Data.Text (Text)
import Language.LoCoEssential.Essence
import Language.LoCoEssential.Interp.Lazy (interpret)
import Language.LoCoEssential.Parse
import Language.LoCoEssential.ParsingExpr.Expr qualified as ParsingExpr
import Language.LoCoEssential.ParsingExpr.Parse qualified as ParsingExpr
import Language.LoCoEssential.SimpleExpr.Expr qualified as SimpleExpr
import Text.Megaparsec (runParser)

smallModule :: LoCoModule SimpleExpr.Expr
smallModule =
  LoCoModule "abc" $
    Map.fromList
      [ ("a", RHSExpr (SimpleExpr.ELit 1)),
        ("b", RHSExpr (SimpleExpr.EAdd (SimpleExpr.EVar "a") (SimpleExpr.EVar "a"))),
        ("c", RHSExpr (error "c"))
      ]

testB :: IO Int
testB =
  do
    m <- interpret smallModule
    b <- m Map.! "b"
    a <- m Map.! "a"
    pure (b + a)

smallParsingModule :: LoCoModule ParsingExpr.Expr
smallParsingModule =
  LoCoModule "parsing" $
    Map.fromList
      [ ("s", RHSExpr (ParsingExpr.ELoad "sample.txt")),
        ("r", RHSExpr (ParsingExpr.ELit (ParsingExpr.VRegion 0 2))),
        ("v", RHSExpr (ParsingExpr.EParse ParsingExpr.Integer (ParsingExpr.EVar "s") (ParsingExpr.EVar "r")))
      ]

testV :: IO (ParsingExpr.Value, ParsingExpr.Value)
testV =
  do
    m <- interpret smallParsingModule
    v <- m Map.! "v"
    v2 <- m Map.! "v"
    pure (v, v2)

parsingModule :: Text
parsingModule = sanitize "parseABC = { src = load(\"sample.txt\") , r = R(0,4) , v = parse(int,src,r) }"

lMod :: LoCoModule ParsingExpr.Expr
lMod =
  case runParser (parseModule ParsingExpr.parseExpr) "<source>" parsingModule of
    Right m -> m
    _ -> undefined

testV2 :: IO ParsingExpr.Value
testV2 =
  do
    m <- interpret lMod
    v <- m Map.! "r"
    pure v