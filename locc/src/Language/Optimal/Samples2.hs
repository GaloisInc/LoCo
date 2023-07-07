{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- {-# OPTIONS_GHC -dsuppress-uniques #-}

module Language.Optimal.Samples2 where

import Data.Word (Word64, Word8)
import Language.Optimal.Quote (optimal)
import Thunk.RefVal (Thunked, delayAction, force)

{- mtg
 - nested structures not supported
-}

---------------------------------------------------------------------------

data T = T Int
         deriving (Eq,Ord,Read,Show)

[optimal|
type LoCo1 = { l1_a : T, l1_b : String }

m1 : LoCo1
m1 = { 
  l1_a = <| putStrLn "l1_a" >> pure (T 500) |>,
  l1_b = <| putStrLn "l1_b" >> case l1_a of T n -> pure (show n) |>
}
|]

---------------------------------------------------------------------------
-- using/testing

userCode1a :: IO (T,String)
userCode1a =
  do
  m1' <- m1
  l1_a' <- force (l1_a m1')
  _    <- force (l1_a m1')
  l1_b' <- force (l1_b m1')
  return (l1_a', l1_b')

userCode1b =
  do
  m1'  <- m1
  l1_b' <- force (l1_b m1')
  _    <- force (l1_b m1')
  return l1_b'


---------------------------------------------------------------------------

l2a' = pure (T 500) :: IO T
l2b' :: T -> IO String
l2b' = f

-- NOTE: if we replace IO with Maybe in last two defns (would type check),
-- then the below 'optimal' fails with a type-mismatch error: good.

[optimal|
type LoCo2 = { l2a : T, l2b : String }

m2 : LoCo2
m2 = { 
  l2a = <| l2a' |>,
  l2b = <| l2b' l2a |>
}
|]

  
---- Foo -----------------------------------------------------------
-- cloned from Samples.hs

largePrime :: Word64
largePrime = 2 ^ (63 :: Word8) - 25

smallerPrime :: Word64
smallerPrime = 2 ^ (24 :: Word8) - 3

facilePrimalityTest :: Word64 -> Bool
facilePrimalityTest n = and [n `mod` i /= 0 | i <- [2 .. n `div` 2]]


[optimal|
type Foo = { a : Bool, b : Char }

foo : Foo
foo = { 
  a = <| pure (facilePrimalityTest smallerPrime) |>,
  b = <| pure (if a then 't' else 'f') |>
}
|]

-- data Foo = Foo
--   { a :: Thunked Bool,
--     b :: Thunked Char
--   }

-- foo :: IO Foo
-- foo =
--   do
--     a <- delayAction (pure (facilePrimalityTest smallerPrime))
--     b <-
--       delayAction
--         ( do
--             a_abKks <- force a
--             pure (if a then 't' else 'f')
--         )
--     pure Foo {a = a, b = b}

userCodeFoo1 :: IO Bool
userCodeFoo1 =
  do
    f <- foo
    let bField :: Thunked Char
        bField = b f
    bPure <- force bField
    a <- force (a f)
    return a
  -- MT: hmmm: sharing more than expected: 2nd call to userCode2 is fast.
  
