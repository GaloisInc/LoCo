module Main where

import Test.Language.LoCoEssential.Essence qualified
import Test.Language.Optimal.Compile qualified
import Test.Language.Optimal.Compile.FreeVars qualified
import Test.Language.Optimal.Parse qualified
import Test.Tasty (TestTree, defaultMain, testGroup)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "main"
    [ Test.Language.LoCoEssential.Essence.tests,
      Test.Language.Optimal.Compile.tests,
      Test.Language.Optimal.Compile.FreeVars.tests,
      Test.Language.Optimal.Parse.tests
    ]
