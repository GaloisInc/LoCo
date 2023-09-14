{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -ddump-splices #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Language.Optimal.Samples.Vector where

import Control.Monad.IO.Class (liftIO)
import Language.Optimal.Quote (optimal)
import Thunk.RefVal
import Thunk.Vector

mkChar :: IO Char
mkChar = liftIO (putStrLn "creating element") >> pure 'a'

[optimal|
type LV = { l : Int, vs : [Char], final : Char }

mkLV : LV
mkLV = {
  l = <| pure 6 |>,
  vs = replicate l <| mkChar |>,
  idx = <| pure (l - 1) |>,
  final = index vs idx
}
|]

chrToInt :: Applicative f => Char -> f Int
chrToInt = pure . fromEnum

[optimal|
type LV2 = { l : Int, cs : [Char], is : [Int], i : Int }

mkLV2 : LV2
mkLV2 = {
  l = <| pure 6 |>,
  cs = replicate l <| mkChar |>,
  is = map cs <| chrToInt |>,
  idx = <| pure (l - 1) |>,
  i = index is idx
}
|]

{-
foo = {
  v = <| e |>
  |      +------ :: m a
  +------------- :: a

-------------------------------------------------------------------------------
-- Introduction

  xs :: Thunked m (Vector m a)

  -- Option 1: thunked length
  xs = replicate len <| e |>
  ||             |||    +----------- :: m a
  ||             +++---------------- :: Integral i => Thunked m i
  ++-------------------------------- :: Thunked m (Vector m a)

  -- Option 2: pure length
  xs = replicate len <| e |>
  ||             |||    +----------- :: m a
  ||             +++---------------- :: Integral i => i
  ++-------------------------------- :: Thunked m (Vector m a)

  xs = unfold len <| e |>
  ||          |||    +-------------- :: a -> m a -- previous to new
  ||          +++------------------- :: Integral i => i
  ++-------------------------------- :: Thunked m (Vector m a)

  xs = unfold len z <| e |>
  ||          ||| |    +------------ :: b -> m (a, b) -- previous to new
  ||          ||| +----------------- :: b
  ||          +++------------------- :: Integral i => i
  ++-------------------------------- :: Thunked m (Vector m a)

-------------------------------------------------------------------------------
-- Transformation

  ys = map xs <| \x -> f v x |>
  ||       ||    +++++++++++----- :: a -> m b
  ||       ++-------------------- :: Thunked m (Vector m a)
  ++----------------------------- :: Thunked m (Vector m b)

-------------------------------------------------------------------------------
-- Elimination

  x :: Thunked m a

  -- Option 1: thunked index
  x = index xs i
  |         || +--------- :: Integral i => Thunked m i
  |         ++----------- :: Thunked m (Vector a)
  +---------------------- :: Thunked m a

  -- Option 2: pure index
  x = index xs i
  |         || +--------- :: Integral i => i
  |         ++----------- :: Thunked m (Vector a)
  +---------------------- :: Thunked m a

}

Concept: we expose (to the user) no interface to `Vec`, and entirely control its
introduction and elimination

This should let us more easily substitute another source language for Haskell
while still reusing the same Optimal syntaxes and constructs

 -}
