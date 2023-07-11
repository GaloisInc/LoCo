module Main where

import Test.Language.LoCoEssential.Essence qualified
import Test.Language.Optimal.Parse qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "main"
    [ Test.Language.LoCoEssential.Essence.tests,
      Test.Language.Optimal.Parse.tests
    ]
