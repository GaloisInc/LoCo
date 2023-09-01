{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Optimal.Typecheck (tests) where

import Language.Optimal.Syntax (Type (..))
import Language.Optimal.Typecheck
import Test.Tasty
import Test.Tasty.HUnit
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ tyExpansionTests
    ]

tyExpansionTests :: TestTree
tyExpansionTests =
  testGroup
    "unaliasing types"
    [ testSuccess "no alias" mempty (Alias fooAlias) (Alias barAlias),
      testSuccess "single-level alias" fooAliasIsFoo (Alias fooAlias) fooTy,
      testSuccess "double-level alias" fooAliasIsBarAlias (Alias barAlias) barTy
    ]
  where
    testSuccess name tyEnv source expected =
      let result = expandType tyEnv source
       in testCase name $ expected @=? result
    fooAlias = "Foo"
    barAlias = "Foo"
    fooTy = Rec fooAlias [("x", Alias "Int")]
    barTy = Rec barAlias [("y", Alias "Char")]
    fooAliasIsFoo = [(fooAlias, fooTy)]
    fooAliasIsBarAlias = [(fooAlias, Alias barAlias), (barAlias, barTy)]
