{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Optimal.Typecheck (tests) where

import Language.Optimal.Syntax (ModuleDecl (..), Type (..), TypeDecl (TypeDecl))
import Language.Optimal.Typecheck
import Test.Tasty
import Test.Tasty.HUnit
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ modTyExpansionTests
    ]

modTyExpansionTests :: TestTree
modTyExpansionTests =
  testGroup
    "unaliasing types"
    [ testSuccess "no alias" mempty fooModule fooTy,
      testSuccess "single-level alias" fooAliasIsFoo (fooModuleWithAlias fooAlias) fooTy,
      testSuccess "double-level alias" fooAliasIsBarAlias (fooModuleWithAlias barAlias) barTy
    ]
  where
    testSuccess name tyEnv source expected =
      let result = expandType tyEnv source
       in testCase name $ expected @=? result
    fooModule =
      ModuleDecl
        { modTy = fooTy,
          modParams = mempty,
          modName = "foo",
          modEnv = mempty
        }
    fooModuleWithAlias alias =
      ModuleDecl
        { modTy = Alias alias,
          modParams = mempty,
          modName = "foo",
          modEnv = mempty
        }
    fooAlias = "Foo"
    barAlias = "Foo"
    fooTy = Rec [("x", Alias "Int")]
    barTy = Rec [("y", Alias "Char")]
    fooAliasIsFoo = [TypeDecl fooAlias fooTy]
    fooAliasIsBarAlias = [TypeDecl fooAlias (Alias barAlias), TypeDecl barAlias barTy]
