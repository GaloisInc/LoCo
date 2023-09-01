{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Optimal.Compile (tests) where

import Control.Exception (SomeException, try)
import Language.Haskell.Meta (parseDecs, parseType)
import Language.Haskell.TH as TH
import Language.Optimal.Compile (compileOptimalType, compileOptimalTypeDecl, compileThunkedOptimalType)
import Language.Optimal.Syntax as Optimal
import Test.Tasty (TestName, TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ typeTests,
      thunkedTypeTests,
      typeDeclTests
    ]

typeTests :: TestTree
typeTests =
  testGroup
    "types"
    [ testSuccess "int" oInt "Int",
      testSuccess "list int" (List oInt) "Vector m Int",
      testSuccess "(int, bool)" (Tuple [oInt, oBool]) "(Int, Bool)",
      testSuccess "int -> bool" (Arrow oInt oBool) "Int -> Bool",
      testFailure "record" (Rec [("x", oInt)])
    ]
  where
    testSuccess = success (compileOptimalType "m") parseType
    testFailure = failure (compileOptimalType "m")

thunkedTypeTests :: TestTree
thunkedTypeTests =
  testGroup
    "thunked types"
    [ testSuccess "int" oInt "Thunked m Int",
      testSuccess "list int" (List oInt) "Thunked m (Vector m Int)",
      testSuccess "(int, bool)" (Tuple [oInt, oBool]) "Thunked m (Int, Bool)",
      testSuccess "int -> bool" (Arrow oInt oBool) "Int -> Thunked m Bool",
      testFailure "record" (Rec [("x", oInt)])
    ]
  where
    testSuccess = success (compileThunkedOptimalType (mkName "m")) parseType
    testFailure = failure (compileThunkedOptimalType (mkName "m"))

typeDeclTests :: TestTree
typeDeclTests =
  testGroup
    "type decls"
    [ testSuccess
        "alias decl"
        (TypeDecl "Foo" oInt)
        "type Foo = Int",
      testSuccess
        "record decl"
        (TypeDecl "Foo" (Rec [("x", oInt)]))
        "data Foo m = Foo {x :: Thunked m Int}"
    ]
  where
    testSuccess = success compileOptimalTypeDecl parseDecs

success ::
  (Eq th, Show th) =>
  (optimal -> Q th) ->
  (String -> Either String th) ->
  TestName ->
  optimal ->
  String ->
  TestTree
success compileOptimal parseTH testName original expected =
  testCase testName $
    case parseTH expected of
      Left err -> assertFailure err
      Right th ->
        do
          actual <- runQ (compileOptimal original)
          th @=? actual

failure ::
  (Eq th, Show th) =>
  (optimal -> Q th) ->
  TestName ->
  optimal ->
  TestTree
failure compileOptimal testName original =
  testCase testName $
    try (runQ (compileOptimal original)) >>= \case
      Left (_ :: SomeException) -> pure ()
      Right _ -> assertFailure ""

oInt :: Optimal.Type
oInt = Alias "Int"

oBool :: Optimal.Type
oBool = Alias "Bool"
