{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Optimal.Parse (tests) where

-- Specialized to Haskell expressions for now

import Data.Text (Text)
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH.Syntax
import Language.Optimal.Parse
import Language.Optimal.Syntax (ModuleBinding (..), Type (..), Pattern (..))
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

testParseSuccess :: (Eq a, Show a) => Parser Exp a -> Text -> a -> Assertion
testParseSuccess p on expected =
  case runParser p parseExp on of
    Left err -> assertFailure err
    Right actual -> actual @?= expected

testParseFailure :: (Eq a, Show a) => Parser Exp a -> Text -> Assertion
testParseFailure p on =
  case runParser p parseExp on of
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
      testSuccess "variable starting with underscore" "_hello" "_hello",
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
      testSuccess "rec" "{ foo : Int, bar : Int }" (Rec tyName [("foo", int), ("bar", int)]),
      testSuccess "with underscore" "Int_2" (Alias "Int_2"),
      testSuccess "list" "[Int]" (List (Alias "Int")),
      testSuccess "list of list" "[[Int]]" (List (List (Alias "Int"))),
      testSuccess "vec" "Vec<Int>" (Vec (Alias "Int")),
      testSuccess "tuple" "(Int,Foo)" (Tuple [Alias "Int", Alias "Foo"]),
      testSuccess "list of tuple" "[(Int,Foo)]" (List (Tuple [Alias "Int", Alias "Foo"])),
      testSuccess "tuple of list" "([Int],Foo)" (Tuple [List (Alias "Int"), Alias "Foo"]),
      testSuccess "tyapp" "List Int" (App (Alias "List") int),
      testFailure "lowercase type" "int",
      testFailure "unterminated record" "{ foo : Int "
    ]
  where
    int = Alias "Int"
    tyName = mempty
    testSuccess name source expected =
      testCase name (testParseSuccess (parseOptimalType tyName) source expected)
    testFailure name source =
      testCase name (testParseFailure (parseOptimalType tyName) source)

exprTests :: TestTree
exprTests =
  testGroup
    "expressions"
    [ testSuccess "literal" "<| 3 |>" (Expression <$> [e|3|]),
      -- This fails because it wants `Just` to parse as qualified (GHC.Maybe.Just)...
      -- testSuccess "application" "<| Just 1 |>" [e|Just 1|],
      testFailure "unterminated" "<| 3",
      testSuccess "literal w/ pure sugar" "{| 3 |}" (Value <$> [e|3|]),
      testFailure "unterminated w/ pure sugar" "{| "
    ]
  where
    testSuccess name source expr =
      testCase name $
        do
          expr' <- runQ expr
          testParseSuccess parseOptimalModuleExpr source expr'
    testFailure name source =
      testCase name (testParseFailure parseOptimalModuleExpr source)

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
        [(Sym "x", Expression (AppE (VarE (mkName "pure")) (LitE (IntegerL 3))))],
      testSuccess
        "one parameter"
        "foo y = { x = <| pure y |> }"
        "foo"
        ["y"]
        [(Sym "x", Expression (AppE (VarE (mkName "pure")) (VarE (mkName "y"))))],
      testSuccess
        "multiple parameters"
        "foo y z = { x = <| pure y |> }"
        "foo"
        ["y", "z"]
        [(Sym "x", Expression (AppE (VarE (mkName "pure")) (VarE (mkName "y"))))]
    ]
  where
    testSuccess name source expectedName expectedParams expectedBinds =
      testCase name (testParseSuccess (parseOptimalModuleBody expectedName) source (expectedParams, expectedBinds))

modBindingTests :: TestTree
modBindingTests =
  testGroup
    "module bindings"
    [ testSuccess
        "value binding"
        "{ x = <| pure 3 |> }"
        [(Sym "x", Expression (AppE (VarE (mkName "pure")) (LitE (IntegerL 3))))],
      testSuccess
        "tuple binding"
        "{ (x, y) = <| pure (3, 4) |> }"
        [(Tup [Sym "x", Sym "y"], Expression (AppE (VarE (mkName "pure")) (TupE [Just (LitE (IntegerL 3)), Just (LitE (IntegerL 4))])))],
      testSuccess
        "nested tuple binding 1"
        "{ (x, (y, z)) = <| pure (3, 4) |> }"
        [(Tup [Sym "x", Tup [Sym "y", Sym "z"]], Expression (AppE (VarE (mkName "pure")) (TupE [Just (LitE (IntegerL 3)), Just (LitE (IntegerL 4))])))],
      testSuccess
        "nested tuple binding 2"
        "{ ((x, y), z) = <| pure (3, 4) |> }"
        [(Tup [Tup [Sym "x", Sym "y"], Sym "z"], Expression (AppE (VarE (mkName "pure")) (TupE [Just (LitE (IntegerL 3)), Just (LitE (IntegerL 4))])))],
      testSuccess
        "vector replicate binding"
        "{ x = replicate l <| pure 3 |> }"
        [(Sym "x", VectorReplicate "l" (AppE (VarE (mkName "pure")) (LitE (IntegerL 3))))],
      testSuccess
        "vector replicate binding from literal"
        "{ x = replicate 3 <| pure 3 |> }"
        [(Sym "x", VectorReplicateLit 3 (AppE (VarE (mkName "pure")) (LitE (IntegerL 3))))],
      testSuccess
        "vector generate binding"
        "{ x = generate l <| pure 3 |> }"
        [(Sym "x", VectorGenerate "l" (AppE (VarE (mkName "pure")) (LitE (IntegerL 3))))],
      testSuccess
        "vector generate binding from literal"
        "{ x = generate 3 <| pure 3 |> }"
        [(Sym "x", VectorGenerateLit 3 (AppE (VarE (mkName "pure")) (LitE (IntegerL 3))))],
      testSuccess
        "multiple bindings"
        "{ x = <| pure 3 |>, y = replicate x <| pure 'a' |> }"
        [ (Sym "x", Expression (AppE (VarE (mkName "pure")) (LitE (IntegerL 3)))),
          (Sym "y", VectorReplicate "x" (AppE (VarE (mkName "pure")) (LitE (CharL 'a'))))
        ],
      testSuccess
        "vector index binding"
        "{ x = index xs i }"
        [(Sym "x", VectorIndex "xs" "i")],
      testSuccess
        "vector index binding from literal"
        "{ x = index xs 3 }"
        [(Sym "x", VectorIndexLit "xs" 3)],
      testSuccess
        "module intro, no args"
        "{ m = module foo }"
        [(Sym "m", ModuleIntro "foo" mempty)],
      testSuccess
        "module intro, args"
        "{ m = module foo x }"
        [(Sym "m", ModuleIntro "foo" ["x"])],
      testSuccess
        "module index"
        "{ f = m.x }"
        [(Sym "f", ModuleIndex "m" "x")],
      testFailure "empty bindings" "{ }"
    ]
  where
    testSuccess name source expected =
      testCase name (testParseSuccess parseOptimalModuleBindings source expected)
    testFailure name source =
      testCase name (testParseFailure parseOptimalModuleBindings source)
