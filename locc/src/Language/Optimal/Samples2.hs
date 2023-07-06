{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- {-# OPTIONS_GHC -dsuppress-uniques #-}

module Language.Optimal.Samples2 where

import Data.Word (Word64, Word8)
import Language.Optimal.Quote (optimal)
import Thunk.RefVal (Thunked, delayAction, force)

largePrime :: Word64
largePrime = 2 ^ (63 :: Word8) - 25

smallerPrime :: Word64
smallerPrime = 2 ^ (24 :: Word8) - 3

facilePrimalityTest :: Word64 -> Bool
facilePrimalityTest n = and [n `mod` i /= 0 | i <- [2 .. n `div` 2]]

{- mtg
 - nested structures
 - ?
-}

---------------------------------------------------------------------------

data T = T Int
         deriving (Eq,Ord,Read,Show)

f l1a = case l1a of T n -> pure $ show n

-- type LoCo1 = { l1_a : T, l1_b : String }
--  FIXME: optimal doesn't support "_"s in label names.

[optimal|
type LoCo1 = { l1a : T, l1b : String }

m1 : LoCo1
m1 = { 
  l1a = <| pure $ T 500 |>,
  l1b = <| f l1a |>
}
|]

--  FIXME: if I inline "f l1a" above, I get this
--   Exception when trying to run compile-time code: ...

userCode1a :: IO (T,String)
userCode1a =
    do
    m1' <- m1
    l1a' <- force (l1a m1')
    l1b' <- force (l1b m1')
    return (l1a', l1b')
  
---- Foo -----------------------------------------------------------
-- cloned from Samples.hs

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
  
