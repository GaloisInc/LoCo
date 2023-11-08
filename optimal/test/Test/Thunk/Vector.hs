{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

{-# HLINT ignore "Use &&" #-}

module Test.Thunk.Vector (tests) where

import Control.Monad (forM)
import Data.IORef (modifyIORef', newIORef, readIORef)
import Data.Vector qualified as V
import Language.Optimal.Quote (optimal)
import Test.QuickCheck (Arbitrary (..), ioProperty, shuffle)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Test.Tasty.QuickCheck (getSize, testProperty)
import Thunk.RefVal
import Thunk.Vector
import Util (moduleName)

tests :: TestTree
tests =
  testGroup
    $moduleName
    [ testAllErrors,
      testHeadError,
      testHeadMapError,
      testMapErrorMasking
    ]

replicateSample :: Int -> IO Int -> IO (Sample IO)
generateSample :: Int -> (Int -> IO Int) -> IO (Sample IO)
mapSample :: (Int -> IO Int) -> Sample IO -> IO (Sample IO)

[optimal|
type Sample = { xs : [Int] }

-- Second argument manifestly *not* an Int
replicateSample : Int -> Int -> Sample
replicateSample len create = {
  xs = replicate len <| create |>
}

-- Second argument manifestly *not* an Int
generateSample : Int -> Int -> Sample
generateSample len create = {
  xs = generate len <| create |>
}

mapSample : Int -> Sample -> Sample
mapSample f sample = {
  s = sample.xs,
  xs = map s <| f |>
}
|]

index :: Thunked IO (Vector IO Int) -> Int -> IO Int
index vecThunk idx =
  do
    vec <- force vecThunk
    valThunk <- vIndex vec idx
    force valThunk

--------------------------------------------------------------------------------

testAllErrors :: TestTree
testAllErrors =
  testCase "forcing a vector doesn't force its elements" $
    do
      xs <- allErrors
      !_ <- force xs
      pure ()

allErrors :: IO (Thunked IO (Vector IO Int))
allErrors =
  do
    sample <- replicateSample 10 (error "allErrors: forced an `error` element")
    pure (xs sample)

--------------------------------------------------------------------------------

testHeadError :: TestTree
testHeadError =
  testCase "forcing one element doesn't force others" $
    do
      xs <- headError
      x <- xs `index` 1
      x @?= 0

-- | The element is 0 divided by the index - since we're 0-indexing, the first
-- element will evaluate to 0/0, which is an error in Haskell
headError :: IO (Thunked IO (Vector IO Int))
headError =
  do
    sample <- generateSample 2 (\i -> pure (0 `div` i))
    pure (xs sample)

--------------------------------------------------------------------------------
-- Forcing one element of a vector transformed via `map` doesn't force others

testHeadMapError :: TestTree
testHeadMapError =
  testCase "mapping preserves element isolation" $
    do
      xs <- headErrorViaMap
      x <- xs `index` 1
      x @?= 0

headErrorViaMap :: IO (Thunked IO (Vector IO Int))
headErrorViaMap =
  do
    sample <- generateSample 2 (\i -> pure (0 `div` i))
    sample' <- mapSample pure sample
    pure (xs sample')

--------------------------------------------------------------------------------
-- Mapping can mask errors

testMapErrorMasking :: TestTree
testMapErrorMasking =
  testCase "mapping masks errors" $
    do
      xs <- maskErrors
      x <- xs `index` 0
      x @?= 42

maskErrors :: IO (Thunked IO (Vector IO Int))
maskErrors =
  do
    sample <- generateSample 2 (\i -> pure (0 `div` i))
    sample' <- mapSample (const (pure 42)) sample
    pure (xs sample')

--------------------------------------------------------------------------------

-- An interesting behavior of vectors - the order of forcing can determine the
-- content of the vector.
--
-- XXX: does this actually "test" any property we'd like not to regress?
testForcingOrder :: TestTree
testForcingOrder =
  testProperty
    "forcing determines content"
    (ioProperty . orderProp)

data Order = Order {oLen :: Int, oOrder :: [Int]}

instance Show Order where
  show Order {..} = show (map showNth oOrder)
    where
      showNth x = show x <> suffix x
      suffix x =
        case x `mod` 100 of
          11 -> "th"
          12 -> "th"
          13 -> "th"
          _ ->
            case x `mod` 10 of
              1 -> "st"
              2 -> "nd"
              3 -> "rd"
              _ -> "th"

instance Arbitrary Order where
  arbitrary =
    do
      x <- getSize
      Order x <$> shuffle [0 .. x - 1]

-- | We can determine a vector's content by the order in which its elements are
-- forced, but only the first time we force them
orderProp :: Order -> IO Bool
orderProp Order {..} =
  do
    vec <- incrVec oLen
    (evalOrder, origOrder) <- evaluate vec oOrder
    (evalOrder', origOrder') <- evaluate vec (reverse oOrder)
    pure $
      and
        [ evalOrder == [0 .. oLen - 1],
          evalOrder == reverse evalOrder',
          origOrder == origOrder'
        ]

-- | Force each element of the vector in the provided order, returning (fst) the
-- elements ordered by when they were evaluated and (snd) the elements ordered
-- by their rank in the vector
evaluate :: Vector IO Int -> [Int] -> IO ([Int], [Int])
evaluate Vector {..} order =
  do
    evalOrder <- forM order $ \idx -> force (vecContent V.! idx)
    origOrder <- V.mapM force vecContent
    pure (evalOrder, V.toList origOrder)

-- | Make a vector of ascending integers
incrVec :: Int -> IO (Vector IO Int)
incrVec len =
  do
    ref <- newIORef 0
    sample <- replicateSample len (incr ref)
    force (xs sample)
  where
    incr ref =
      do
        val <- readIORef ref
        modifyIORef' ref (+ 1)
        pure val
