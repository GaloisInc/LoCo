{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Language.Optimal.Compile.Free (tests) where

import Data.String (IsString (..))
import Language.Haskell.TH (Name, mkName, runQ)
import Language.Optimal.Compile.Free (freeVars)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ exprFreeVarTests
    ]

exprFreeVarTests :: TestTree
exprFreeVarTests =
  testGroup
    "free variables"
    [ expr "literal" [|3|] mempty,
      expr "unbound var" [|x|] ["x"],
      expr "con" [|True|] mempty,
      expr "app" [|Just x y|] ["x", "y"],
      expr "tyapp" [|x @Int|] ["x"],
      expr "infix" [|x `y` z|] ["x", "y", "z"],
      expr "parens" [|(x)|] ["x"],
      expr "identity lambda" [|\x -> x|] mempty,
      expr "lambda w/ free" [|\x -> y|] ["y"],
      expr "tuple" [|(x, y)|] ["x", "y"],
      expr "unboxed tuple" [|(# x, y #)|] ["x", "y"],
      expr "cond" [|if x then y else z|] ["x", "y", "z"],
      expr "multi-way-if, simple" [|if | x -> y|] ["x", "y"],
      expr "multi-way-if, shadow" [|if | x -> x|] ["x"],
      expr "multi-way-if, pattern" [|if | (x, y) <- z -> w|] ["z", "w"],
      expr "list" [|[x, y]|] ["x", "y"],
      expr "let, no frees" [|let x = 3 in x|] mempty,
      expr "let, free in binding" [|let x = y in x|] ["y"],
      expr "let, free in body" [|let x = 3 in y|] ["y"],
      expr "let, where-binding" [|let x = y where y = 3 in x|] mempty,
      expr "let, recursive" [|let x = x in x|] mempty,
      expr "case, simple" [|case x of _ -> y|] ["x", "y"],
      expr "case, shadow" [|case x of y -> y|] ["x"],
      expr "case, pattern shadow" [|case x of Just y -> y|] ["x"],
      expr "case, guarded" [|case x of _ | Just y <- z -> y|] ["x", "z"],
      expr "do, singleton" [|do x|] ["x"],
      expr "do, multi" [|do x; y|] ["x", "y"],
      expr "do, bind" [|do x <- y; x|] ["y"],
      expr "sig" [|x :: Int|] ["x"]
    ]
  where
    expr testName e expectedFrees =
      testCase testName $
        do
          e' <- runQ e
          let actualFrees = freeVars e'
          actualFrees @?= expectedFrees

instance IsString Name where
  fromString = mkName
