{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
  -- alternatively we can require user to provide a signature for their
  -- generated optimal module.
{-# OPTIONS_GHC -ddump-splices #-}

-- {-# OPTIONS_GHC -dsuppress-uniques #-}

module Language.Optimal.Samples2 where

import Control.Monad.IO.Class
import Control.Monad.Trans.Except
import Data.Word (Word64, Word8)

import Language.Optimal.Quote (optimal)
import Thunk.RefVal (Thunked, delayAction, force)

{- mtg
 - nested structures not supported
-}

---------------------------------------------------------------------------

data T = T Int
         deriving (Eq,Ord,Read,Show)

f :: Applicative f => T -> f String
f l1_a = case l1_a of T n -> pure (show n)

m1 :: MonadIO m => m (LoCo1 m)
[optimal|
type LoCo1 = { l1_a : T, l1_b : String }

m1 : LoCo1
m1 = { 
  l1_a = <| liftIO (putStrLn "l1_a" >> pure (T 500) )|>,
  l1_b = <| liftIO (putStrLn "l1_b" >> case l1_a of T n -> pure (show n)) |>
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

userCode1b :: IO String
userCode1b =
  do
  m1'  <- m1
  l1_b' <- force (l1_b m1')
  _    <- force (l1_b m1')
  return l1_b'

---------------------------------------------------------------------------

l2a' :: MonadIO m => m T
l2a' = pure (T 500)
l2b' :: MonadIO m => T -> m String
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

m2 :: MonadIO m => m (LoCo2 m)

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

foo :: MonadIO m => m (Foo m)

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
    x <- foo
    _b <- force (b x)
    a <- force (a x)
    return a
  -- MT: hmmm: sharing more than expected: 2nd call to userCode2 is fast.
  --  - saw in ghci; appears it's a ghci thing.


---- Testing the overloaded Optimal --------------------------------


[optimal|
type LoCo3 = { l3a : Int, l3b : Int, l3c : Int }
m3 : LoCo3
m3 = { 
     l3a = <| pure 5 |>,
     l3b = <| if even l3a then return (l3a+1) else throwE ["odd"] |>,
     l3c = <| pure (l3a + 2) |>
}
|]

type E m = ExceptT [String] m
m3 :: MonadIO m => E m (LoCo3 (E m))
  -- NOTE: because we are giving this a signature; we might have have
  -- dispensed with NoMonomorphismRestriction.
  
-- functional version:
m3_Hs :: (Monad m) => ExceptT [String] m (Int, Int)
m3_Hs = do
         a <- return (5 :: Int)
         b <- if even a then return (a+1)
                        else throwE ["odd"]
         return (a,b)

userCodeM3a :: MonadIO m => m (Either [String] (Int, Int))
userCodeM3a = runExceptT $
  do
    m' <- m3
    a <- force (l3a m')
    -- b <- force (l3b m')
    c <- force (l3c m')
    _a2 <- force (l3a m')
    return (a,c)

userCodeM3b :: MonadIO m => m (Either [String] (Int, Int))
userCodeM3b = runExceptT $
  do
    m' <- m3
    a <- force (l3a m')
    _c <- force (l3c m')
    b <- force (l3b m')
    return (a,b)

