{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Language.Optimal.Compile.Rename (tests) where

import Data.String (IsString (..))
import Language.Haskell.TH
import Language.Optimal.Compile.Rename (rename)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@=?))
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ exprRenameTests
    ]

-- | The renaming strategy of these tests is to rename variables named "x" to
-- variables named "y", where appropriate
exprRenameTests :: TestTree
exprRenameTests =
  testGroup
    "renaming"
    [ sameExpr "literal" [|3|],
      sameExpr "irrelevant var" [|y|],
      expr
        "relevant var"
        [|x|]
        [|y|],
      sameExpr "con" [|True|],
      expr
        "app"
        [|Just x y|]
        [|Just y y|],
      expr
        "tyapp"
        [|x @Int|]
        [|y @Int|],
      expr
        "infix"
        [|x `y` z|]
        [|y `y` z|],
      expr
        "parens"
        [|(x)|]
        [|(y)|],
      sameExpr "identity lambda" [|\x -> x|],
      sameExpr "lambda with x parameter" [|\x -> y|],
      -- sameExpr "lambda with unbound" [|\y -> x|],
      expr
        "tuple"
        [|(x, y)|]
        [|(y, y)|],
      expr
        "unboxed tuple"
        [|(# x, y #)|]
        [|(# y, y #)|],
      expr
        "cond"
        [|if x then y else z|]
        [|if y then y else z|],
      expr
        "multi-way-if, simple"
        [|if | x -> y|]
        [|if | y -> y|],
      expr
        "multi-way-if, shadow"
        [|if | x -> x|]
        [|if | y -> y|],
      sameExpr "multi-way-if, pattern" [|if | (x, _) <- z -> x|],
      expr
        "list"
        [|[x, y]|]
        [|[y, y]|],
      sameExpr "let, bound x" [|let x = 3 in x|],
      expr
        "let, free x"
        [|let $(varP "z") = x in $(varE "z")|]
        [|let $(varP "z") = y in $(varE "z")|],
      expr
        "let, x free in where"
        [|let $(varP "z") = $(varE "y") where $(varP "y") = x in $(varE "z")|]
        [|let $(varP "z") = $(varE "y") where $(varP "y") = y in $(varE "z")|],
      sameExpr "let, x bound in where" [|let z = x where x = y in z|],
      expr
        "case, x in case position"
        [|case x of _ -> y|]
        [|case y of _ -> y|],
      expr
        "case, x in case position, bound match body"
        [|case x of $(varP "y") -> $(varE "y")|]
        [|case y of $(varP "y") -> $(varE "y")|],
      expr
        "case, x free in match position"
        [|case y of _ -> x|]
        [|case y of _ -> y|],
      sameExpr "case, x bound in match position" [|case y of x -> x|],
      expr
        "case, guarded"
        [|case x of _ | Just $(varP "y") <- z -> $(varE "y")|]
        [|case y of _ | Just $(varP "y") <- z -> $(varE "y")|],
      expr
        "do, singleton"
        [|do x|]
        [|do y|],
      expr
        "do, multi"
        [|do x; y|]
        [|do y; y|],
      sameExpr "do, x bound" [|do x <- y; x|],
      expr
        "sig"
        [|x :: Int|]
        [|y :: Int|]
    ]
  where
    sameExpr testName original =
      testCase testName $
        do
          original' <- runQ original
          let actual = rename xBecomesY original'
          original' @=? actual

    expr testName original expected =
      testCase testName $
        do
          original' <- runQ original
          expected' <- runQ expected
          let actual = rename xBecomesY original'
          expected' @=? actual

    xBecomesY n =
      if n == "x"
        then "y"
        else n

instance IsString Name where
  fromString = mkName
