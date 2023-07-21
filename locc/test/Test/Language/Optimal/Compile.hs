{-# LANGUAGE TemplateHaskell #-}

module Test.Language.Optimal.Compile (tests) where

import Test.Tasty (TestTree, testGroup)
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    []
