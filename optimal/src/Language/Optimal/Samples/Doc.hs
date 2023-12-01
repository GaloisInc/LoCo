{-# OPTIONS_GHC -Wno-type-defaults #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Language.Optimal.Samples.Doc where

import Control.Monad.IO.Class (MonadIO (..))
import Language.Optimal.Quote (optimal)
import Thunk.RefVal (Thunked, delayAction, force)
import Thunk.Vector

isPrime :: Int -> Bool
isPrime n =
  and
    [ n `mod` i /= 0
      | i <- [2 .. ceiling (sqrt (fromIntegral n))]
    ]

[optimal|
type Foo = { x : Bool, y : Char }

foo : Foo
foo = {
  x = <| pure (isPrime 30000001) |>,
  y = <| pure (if x then 'T' else 'F') |>,
}
|]

foo :: MonadIO m => m (Foo m)
-- x :: MonadIO m => Foo m -> Thunked m Int
-- y :: MonadIO m => Foo m -> m (Thunked m Bool)

workWithFoo :: IO ()
workWithFoo =
  do
    f <- foo

    let xThunk :: Thunked IO Bool
        xThunk = x f
        yThunk :: Thunked IO Char
        yThunk = y f

    force xThunk >>= print
    force yThunk >>= print

[optimal|
type Bar = { xs : Vec<Bool> }

bar : Bar
bar = {
  xs = generate 100 <| \idx -> pure (isPrime idx) |>,
}

bar' : Bar
bar' = {
  xsLen = <| pure 100 |>,
  xs = generate xsLen <| \idx -> pure (isPrime idx) |>,
}
|]

bar :: MonadIO m => m (Bar m)
bar' :: MonadIO m => m (Bar m)
-- xs :: Bar m -> Thunked m (Vector m Bool)

workWithBar :: IO ()
workWithBar =
  do
    b <- bar

    let xsThunk :: Thunked IO (Vector IO Bool)
        xsThunk = xs b

    xsVec <- force xsThunk

    vIndex xsVec 0 >>= print
