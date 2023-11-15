module Main where

import Test.Language.Optimal.Compile qualified
import Test.Language.Optimal.Compile.Free qualified
import Test.Language.Optimal.Compile.Rename qualified
import Test.Language.Optimal.Parse qualified
import Test.Language.Optimal.Typecheck qualified
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Thunk.Vector qualified
import Util (moduleName)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ Test.Language.Optimal.Compile.tests,
      Test.Language.Optimal.Compile.Free.tests,
      Test.Language.Optimal.Compile.Rename.tests,
      Test.Language.Optimal.Parse.tests,
      Test.Language.Optimal.Typecheck.tests,
      Test.Thunk.Vector.tests
    ]
