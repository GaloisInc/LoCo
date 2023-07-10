{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- {-# OPTIONS_GHC -dsuppress-uniques #-}

module Language.Optimal.Samples2 where

import Control.Monad.IO.Class
import Control.Monad.Trans.Identity
import Data.Word (Word64, Word8)

import Language.Optimal.Quote (optimal)
import Thunk.RefVal (Thunked, delayAction, force)

{- mtg
 - nested structures not supported
-}

---------------------------------------------------------------------------

data T = T Int
         deriving (Eq,Ord,Read,Show)

f l1_a = case l1_a of T n -> pure (show n)

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
    x <- foo
    b <- force (b x)
    a <- force (a x)
    return a
  -- MT: hmmm: sharing more than expected: 2nd call to userCode2 is fast.
  --  - saw in ghci; appears it's a ghci thing.
  
  
---- exploring the generalization of types -------------------------

-- currently generated:

-- data Foo1 = Foo1 {a1 :: (Thunked Bool), b1 :: (Thunked Char)}
-- foo1 :: IO Foo1
-- foo1 = do a <- delayAction (pure (facilePrimalityTest smallerPrime))
--           b <- delayAction
--                  (do a_ag32 <- force a
--                      pure (if a_ag32 then 't' else 'f'))
--           pure Foo1 {a1 = a, b1 = b}

-- TODO:FEATURE-REQUEST
-- want this to be generated:

data Foo1 m = Foo1 {a1 :: (Thunked m Bool), b1 :: (Thunked m Char)}

foo2 :: MonadIO m => m (Foo1 m)
foo2 = do a <- delayAction' (pure (facilePrimalityTest smallerPrime))
          b <- delayAction'
                 (do a_ag32 <- force' a
                     pure (if a_ag32 then 't' else 'f'))
          pure Foo1 {a1 = a, b1 = b}
          
          
-- where we have these overloaded variants:
--   (similiar to what is in tinman/LoCo/MEP/Thunk.hs)

delayAction' :: MonadIO m => m a -> m (Thunked m a)
delayAction' m = error "delayAction'"
                  
force' :: MonadIO m => Thunked m a -> m a
force' x = error "force'"


-- how we could use foo2:

-- [Sam] I think the 'proof' you want to have is more easily demonstrated by
-- wrapping the whole computation in `runIdentityT`, rather than just the `foo2`
-- computation (which would entail wrapping the `force`s similarly)
userCodeFoo2 :: IO Bool
userCodeFoo2 = runIdentityT $
  do
    x <- foo2
         -- 'proof' that we can use foo2 at other MonadIO instances.
    b <- force (b1 x)
    a <- force (a1 x)
    return a
  
