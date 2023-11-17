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
      testSuccess "vec int" (Vec oInt) "Vector m Int",
      testSuccess "list int" (List oInt) "[Int]",
      testSuccess "(int, bool)" (Tuple [oInt, oBool]) "(Int, Bool)",
      testSuccess "int -> bool" (Arrow oInt oBool) "Int -> Bool",
      testSuccess "record" (Rec "Foo" [("x", oInt)]) "Foo m"
    ]
  where
    testSuccess = success (compileOptimalType mempty "m") parseType

thunkedTypeTests :: TestTree
thunkedTypeTests =
  testGroup
    "thunked types"
    [ testSuccess "int" oInt "Thunked m Int",
      testSuccess "vec int" (Vec oInt) "Thunked m (Vector m Int)",
      testSuccess "list int" (List oInt) "Thunked m [Int]",
      testSuccess "(int, bool)" (Tuple [oInt, oBool]) "Thunked m (Int, Bool)",
      testSuccess "int -> bool" (Arrow oInt oBool) "Int -> Thunked m Bool",
      testSuccess "record" (Rec "Foo" [("x", oInt)]) "Thunked m (Foo m)"
    ]
  where
    testSuccess = success (compileThunkedOptimalType mempty "m") parseType

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
        (TypeDecl "Foo" (Rec "Foo" [("x", oInt)]))
        "data Foo m = Foo {x :: Thunked m Int}"
    ]
  where
    testSuccess = success (compileOptimalTypeDecl mempty "m") parseDecs

-- | Generic method for testing compilation
success ::
  (Eq th, Show th) =>
  (optimal -> Q th) -> -- ^ compile something to a TH value
  (String -> Either String th) -> -- ^ parse a TH value
  TestName ->
  optimal -> -- ^ something to compile
  String -> -- ^ what it should compile to (in regular Haskell concrete syntax)
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
