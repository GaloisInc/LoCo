{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Optimal.Parse (tests) where

import Data.Map qualified as Map
import Data.Text (Text)
import Language.Optimal.Parse
import Language.Optimal.Syntax (Type (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ varNameTests,
      typeTests
    ]

testParseSuccess :: (Eq a, Show a) => Parser a -> Text -> a -> Assertion
testParseSuccess p on expected =
  case runParser p on of
    Left err -> assertFailure err
    Right actual -> actual @?= expected

testParseFailure :: (Eq a, Show a) => Parser a -> Text -> Assertion
testParseFailure p on =
  case runParser p on of
    Left err -> pure ()
    Right actual -> assertFailure "parsing succeeded"

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
      testFailure "lowercase type" "int",
      testFailure "unterminated record" "{ foo : Int "
    ]
  where
    int = Alias "Int"
    testSuccess name source expected =
      testCase name (testParseSuccess parseOptimalType source expected)
    testFailure name source =
      testCase name (testParseFailure parseOptimalType source)
