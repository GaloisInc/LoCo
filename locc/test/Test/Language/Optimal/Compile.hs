{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Test.Language.Optimal.Compile (tests) where

import Data.String (IsString (..))
import Language.Haskell.TH (Name, mkName, runQ)
import Language.Optimal.Compile (freeVars)
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
      expr "lambda w/ bound" [|\x -> x|] mempty,
      expr "lambda w/ unbound" [|\_x -> y|] ["y"],
      expr "tuple" [|(x, y)|] ["x", "y"],
      expr "unboxed tuple" [|(# x, y #)|] ["x", "y"],
      expr "cond" [|if x then y else z|] ["x", "y", "z"],
      expr "multi-way-if, simple" [|if | x -> y|] ["x", "y"],
      expr "multi-way-if, shadow" [|if | x -> x|] ["x"],
      expr "multi-way-if, pattern" [|if | (x, _y) <- z -> x|] ["z"],
      expr "list" [|[x, y]|] ["x", "y"],
      expr "let, simple" [|let x = 3 in x|] mempty,
      expr "let, recurse" [|let x = x in x|] mempty,
      expr "case, simple" [|case x of _ -> y|] ["x", "y"],
      expr "case, shadow" [|case x of y -> y|] ["x"],
      expr "case, pattern shadow" [|case x of Just y -> y|] ["x"],
      expr "case, guarded" [|case x of _ | Just y <- z -> y|] ["x", "z"],
      expr "do, singleton" [|do x|] ["x"],
      expr "do, multi" [|do x; y|] ["x", "y"],
      expr "do, bind" [|do x <- y; x|] ["y"]
    ]
  where
    expr name e expectedFrees =
      testCase name $
        do
          e' <- runQ e
          let actualFrees = freeVars e'
          actualFrees @?= expectedFrees

instance IsString Name where
  fromString = mkName
