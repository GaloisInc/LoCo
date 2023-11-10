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

mkChar' :: Int -> IO Char
mkChar' idx = liftIO (putStrLn ("creating " <> show idx <> " element") >> pure (toEnum idx))

sampleLength :: Applicative f => f Int
sampleLength = pure 5

[optimal|
type Str = { chars : [Char] }

-- Replication from a thunked length
replicateStrThunked : Str
replicateStrThunked = {
  len = <| sampleLength |>,
  chars = replicate len <| mkChar |>
}

-- Replication from a pure length
replicateStrPure : Int -> Str
replicateStrPure len = {
  chars = replicate len <| mkChar |>
}

-- Generation from a thunked length
generateStrThunked : Str
generateStrThunked = {
  len = <| sampleLength |>,
  chars = generate len <| mkChar' |>
}

-- Generation from a pure length
generateStrPure : Int -> Str
generateStrPure len = {
  chars = generate len <| mkChar' |>
}

type Chr = { c : Char }

-- Indexing from a thunked index
indexStrThunked : Chr
indexStrThunked = {
  len = <| sampleLength |>,
  vs = replicate len <| mkChar |>,
  idx = <| pure 4 |>,
  c = index vs idx
}

-- Indexing from a pure index
indexStrPure : Int -> Chr
indexStrPure idx = {
  len = <| pure (idx + 1) |>,
  vs = replicate len <| mkChar |>,
  c = index vs idx
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
