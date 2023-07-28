{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Optimal.Compile (tests) where

import Control.Exception (SomeException, try)
import Language.Haskell.TH
import Language.Optimal.Compile (compileOptimalType)
import Language.Optimal.Syntax
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase, (@=?))
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ typeTests
    ]

typeTests :: TestTree
typeTests =
  testGroup
    "types"
    [ testSuccess "int" oInt hInt,
      testSuccess "list int" (List oInt) [t|[$hInt]|],
      testSuccess "(int, bool)" (Tuple [oInt, oBool]) [t|($hInt, $hBool)|],
      testSuccess "int -> bool" (Arrow oInt oBool) [t|$hInt -> $hBool|],
      testFailure "record" (Rec [("x", oInt)])
    ]
  where
    oInt = Alias "Int"
    oBool = Alias "Bool"

    -- Why not just say [t|<type>|]? Because that gets interpreted as
    -- `GHC.Types.<type>`, which compares unequally with `<type>`.
    hInt = conT (mkName "Int")
    hBool = conT (mkName "Bool")

    testSuccess name ty expected =
      testCase name $
        do
          expected' <- runQ expected
          actual <- runQ (compileOptimalType ty)
          expected' @=? actual
    testFailure name ty =
      testCase name $
        try (runQ (compileOptimalType ty)) >>= \case
          Left (_ :: SomeException) -> pure ()
          Right _ -> assertFailure ""
