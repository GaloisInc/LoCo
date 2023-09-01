{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Optimal.Parse (tests) where

import Data.Map qualified as Map
import Data.Text (Text)
import Language.Haskell.TH.Syntax
import Language.Optimal.Parse
import Language.Optimal.Syntax (ModuleBinding (..), ModuleDecl (..), Type (..))
import Language.Optimal.Typecheck (expandType)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase, (@?=))
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ varNameTests,
      typeTests,
      exprTests,
      modTypeTests,
      modBindingTests,
      modBodyTests
      -- modTyUnaliasTests
    ]

testParseSuccess :: (Eq a, Show a) => Parser a -> Text -> a -> Assertion
testParseSuccess p on expected =
  case runParser p on of
    Left err -> assertFailure err
    Right actual -> actual @?= expected

testParseFailure :: (Eq a, Show a) => Parser a -> Text -> Assertion
testParseFailure p on =
  case runParser p on of
    Left _err -> pure ()
    Right _ -> assertFailure "parsing succeeded when it was expected to fail"

varNameTests :: TestTree
varNameTests =
  testGroup
    "variable names"
    [ testSuccess "lowercase variable" "hello" "hello",
      testSuccess "variable with internal uppercase" "hEllo" "hEllo",
      testSuccess "variable with numerals" "hell0" "hell0",
      testSuccess "variable with underscores" "he_llo" "he_llo",
      testFailure "variable with initial uppercase" "Hello"
    ]
  where
    testSuccess name source expected =
      testCase name (testParseSuccess parseVarName source expected)
    testFailure name source =
      testCase name (testParseFailure parseVarName source)

typeTests :: TestTree
typeTests =
  testGroup
    "types"
    [ testSuccess "int" "Int" int,
      testSuccess "fn2" "Int -> Int" (Arrow int int),
      testSuccess "fn3" "Int -> Int -> Int" (Arrow int (Arrow int int)),
      testSuccess "rec" "{ foo : Int, bar : Int }" (Rec (Map.fromList [("foo", int), ("bar", int)])),
      testSuccess "with underscore" "Int_2" (Alias "Int_2"),
      testSuccess "list" "[Int]" (List (Alias "Int")),
      testSuccess "list of list" "[[Int]]" (List (List (Alias "Int"))),
      testSuccess "tuple" "(Int,Foo)" (Tuple [Alias "Int", Alias "Foo"]),
      testSuccess "list of tuple" "[(Int,Foo)]" (List (Tuple [Alias "Int", Alias "Foo"])),
      testSuccess "tuple of list" "([Int],Foo)" (Tuple [List (Alias "Int"), Alias "Foo"]),
      testFailure "lowercase type" "int",
      testFailure "unterminated record" "{ foo : Int "
    ]
  where
    int = Alias "Int"
    testSuccess name source expected =
      testCase name (testParseSuccess parseOptimalType source expected)
    testFailure name source =
      testCase name (testParseFailure parseOptimalType source)

exprTests :: TestTree
exprTests =
  testGroup
    "expressions"
    [ testSuccess "literal" "<| 3 |>" [e|3|],
      -- This fails because it wants `Just` to parse as qualified (GHC.Maybe.Just)...
      -- testSuccess "application" "<| Just 1 |>" [e|Just 1|],
      testFailure "unterminated" "<| 3"
    ]
  where
    testSuccess name source expr =
      testCase name $
        do
          expr' <- runQ expr
          testParseSuccess parseValExpr source expr'
    testFailure name source =
      testCase name (testParseFailure parseValExpr source)

modTypeTests :: TestTree
modTypeTests =
  testGroup
    "module types"
    [ testSuccess "simple type ascription" "foo : Foo" ("foo", Alias "Foo"),
      testFailure "lowercase type ascription" "foo : foo"
    ]
  where
    testSuccess name source expected =
      testCase name (testParseSuccess parseOptimalModuleTypeAscription source expected)
    testFailure name source =
      testCase name (testParseFailure parseOptimalModuleTypeAscription source)

modBodyTests :: TestTree
modBodyTests =
  testGroup
    "module bodies"
    [ testSuccess
        "no parameters"
        "foo = { x = <| pure 3 |> }"
        "foo"
        mempty
        [("x", ValueBinding (AppE (VarE (mkName "pure")) (LitE (IntegerL 3))))],
      testSuccess
        "one parameter"
        "foo y = { x = <| pure y |> }"
        "foo"
        ["y"]
        [("x", ValueBinding (AppE (VarE (mkName "pure")) (VarE (mkName "y"))))],
      testSuccess
        "multiple parameters"
        "foo y z = { x = <| pure y |> }"
        "foo"
        ["y", "z"]
        [("x", ValueBinding (AppE (VarE (mkName "pure")) (VarE (mkName "y"))))]
    ]
  where
    testSuccess name source expectedName expectedParams expectedBinds =
      testCase name (testParseSuccess (parseOptimalModuleBody expectedName) source (expectedParams, expectedBinds))

modBindingTests :: TestTree
modBindingTests =
  testGroup
    "module bindings"
    [ testSuccess
        "single value binding"
        "{ x = <| pure 3 |> }"
        [("x", ValueBinding (AppE (VarE (mkName "pure")) (LitE (IntegerL 3))))],
      testSuccess
        "single vector binding"
        "{ x = replicate l <| pure 3 |> }"
        [("x", VectorBinding "l" (AppE (VarE (mkName "pure")) (LitE (IntegerL 3))))],
      testSuccess
        "multiple bindings"
        "{ x = <| pure 3 |>, y = replicate x <| pure 'a' |> }"
        [ ("x", ValueBinding (AppE (VarE (mkName "pure")) (LitE (IntegerL 3)))),
          ("y", VectorBinding "x" (AppE (VarE (mkName "pure")) (LitE (CharL 'a'))))
        ],
      testSuccess
        "vector index binding"
        "{ x = index xs i }"
        [("x", IndexBinding "xs" "i")],
      testFailure "empty bindings" "{ }"
    ]
  where
    testSuccess name source expected =
      testCase name (testParseSuccess parseOptimalModuleBindings source expected)
    testFailure name source =
      testCase name (testParseFailure parseOptimalModuleBindings source)
