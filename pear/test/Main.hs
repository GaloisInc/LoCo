module Main where

import Language.OptimalPEAR.Examples.ICC_Tests

main :: IO ()
main =
  do
  -- testing PEAR (not Optimal):
  putStrLn "run good tests:"
  runtestsG

  putStrLn "run error-producing tests:"
  runtestsE

  -- FIXME: add tests for Optimal
