{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Optimal.Parse (tests) where

import Data.Map qualified as Map
import Data.Text (Text)
import Language.Haskell.TH.Syntax
import Language.Optimal.Parse
import Language.Optimal.Syntax (ModuleDecl (..), Type (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
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
      modTyUnaliasTests
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
          testParseSuccess parseHSExpr source expr'
    testFailure name source =
      testCase name (testParseFailure parseHSExpr source)

modTypeTests :: TestTree
modTypeTests =
  testGroup
    "module types"
    [ testSuccess "simple type ascription" "foo : Foo" ("foo", "Foo"),
      testFailure "lowercase type ascription" "foo : foo"
    ]
  where
    testSuccess name source expected =
      testCase name (testParseSuccess parseOptimalTypeAscription source expected)
    testFailure name source =
      testCase name (testParseFailure parseOptimalTypeAscription source)

modBindingTests :: TestTree
modBindingTests =
  testGroup
    "module bindings"
    [ testSuccess
        "single binding"
        "{ x = <| pure 3 |> }"
        [("x", AppE (VarE (mkName "pure")) (LitE (IntegerL 3)))],
      testSuccess
        "multiple bindings"
        "{ x = <| pure 3 |>, y = <| pure 'a' |> }"
        [ ("x", AppE (VarE (mkName "pure")) (LitE (IntegerL 3))),
          ("y", AppE (VarE (mkName "pure")) (LitE (CharL 'a')))
        ],
      testFailure "empty bindings" "{ }"
    ]
  where
    testSuccess name source expected =
      testCase name (testParseSuccess parseOptimalModuleBindings source expected)
    testFailure name source =
      testCase name (testParseFailure parseOptimalModuleBindings source)

modTyUnaliasTests :: TestTree
modTyUnaliasTests =
  testGroup
    "unaliasing types"
    [ testSuccess "no alias" mempty fooModule fooTy,
      testSuccess "single-level alias" fooAliasIsFoo (fooModuleWithAlias fooAlias) fooTy,
      testSuccess "double-level alias" fooAliasIsBarAlias (fooModuleWithAlias barAlias) barTy
    ]
  where
    testSuccess name tyEnv source expected =
      let result = unaliasTypes tyEnv [source]
       in testCase name $
            case result of
              Left err -> assertFailure err
              Right [ModuleDecl {..}] -> expected @=? modTy
              Right actuals -> assertFailure "this shouldn't happen"
    fooModule =
      ModuleDecl {modTyName = fooAlias, modTy = fooTy, modName = "foo", modEnv = mempty}
    fooModuleWithAlias alias =
      ModuleDecl {modTyName = alias, modTy = Alias alias, modName = "foo", modEnv = mempty}
    fooAlias = "Foo"
    barAlias = "Foo"
    fooTy = Rec [("x", Alias "Int")]
    barTy = Rec [("y", Alias "Char")]
    fooAliasIsFoo = [(fooAlias, fooTy)]
    fooAliasIsBarAlias = [(fooAlias, Alias barAlias), (barAlias, barTy)]
