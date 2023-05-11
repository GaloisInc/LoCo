module Language.LoCoEssential.Samples where

import Data.Map qualified as Map
import Language.LoCoEssential.Essence
import Language.LoCoEssential.Expr qualified as Expr
import Language.LoCoEssential.ExprParse qualified as ExprParse
import Language.LoCoEssential.Interp.Lazy (interpret)

smallModule :: LoCoModule Expr.Expr
smallModule =
  LoCoModule "abc" $
    Map.fromList
      [ ("a", RHSExpr (Expr.ELit 1)),
        ("b", RHSExpr (Expr.EAdd (Expr.EVar "a") (Expr.EVar "a"))),
        ("c", RHSExpr (error "c"))
      ]

testB :: IO Int
testB =
  do
    m <- interpret smallModule
    b <- m Map.! "b"
    a <- m Map.! "a"
    pure (b + a)

smallParsingModule :: LoCoModule ExprParse.Expr
smallParsingModule =
  LoCoModule "parsing" $
    Map.fromList
      [ ("s", RHSExpr (ExprParse.ELoad "sample.txt")),
        ("r", RHSExpr (ExprParse.ELit (ExprParse.VRegion 0 2))),
        ("v", RHSExpr (ExprParse.EParse ExprParse.Integer (ExprParse.EVar "s") (ExprParse.EVar "r")))
      ]

testV :: IO (ExprParse.Value, ExprParse.Value)
testV =
  do
    m <- interpret smallParsingModule
    v <- m Map.! "v"
    v2 <- m Map.! "v"
    pure (v, v2)