module Main where

import Test.Language.LoCoEssential.Essence qualified
import Test.Language.Optimal.Compile qualified
import Test.Language.Optimal.Compile.Haskell.Free qualified
import Test.Language.Optimal.Compile.Haskell.Rename qualified
import Test.Language.Optimal.Parse qualified
import Test.Language.Optimal.Typecheck qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Thunk.Vector qualified

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "main"
    [ Test.Language.LoCoEssential.Essence.tests,
      Test.Language.Optimal.Compile.tests,
      Test.Language.Optimal.Compile.Haskell.Free.tests,
      Test.Language.Optimal.Compile.Haskell.Rename.tests,
      Test.Language.Optimal.Parse.tests,
      Test.Language.Optimal.Typecheck.tests,
      Test.Thunk.Vector.tests
    ]
