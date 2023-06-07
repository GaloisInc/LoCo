{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

-- {-# OPTIONS_GHC -dsuppress-uniques #-}

module Language.Optimal.Samples where

import Data.Word (Word64, Word8)
import Language.Optimal.Quote (optimal)
import Thunk.RefVal

largePrime :: Word64
largePrime = 2 ^ (63 :: Word8) - 25

smallerPrime :: Word64
smallerPrime = 2 ^ (24 :: Word8) - 3

facilePrimalityTest :: Word64 -> Bool
facilePrimalityTest n = and [n `mod` i /= 0 | i <- [2 .. n `div` 2]]

-------------------------------------------------------------------------------

[optimal|
type Foo = { a : Bool, b : Char }
|]

-- data Foo = Foo
--   { a :: Thunked Bool,
--     b :: Thunked Char
--   }

[optimal|
type Bar = { bar : Foo, b2 : Bool }
|]

-- data Bar = Bar
--   { bar :: Thunked Foo,
--     b2 :: Thunked Bool
--   }

[optimal|
foo : Foo
foo = { 
  a = <| pure (facilePrimalityTest smallerPrime) |>,
  b = <| pure (if a then 't' else 'f') |>
}
|]

-- foo' :: IO Foo
-- foo' =
--   do
--     a <- delayAction (pure (isPrime largePrime))
--     b <-
--       delayAction
--         ( do
--             a_abKks <- force a
--             pure (if a then 't' else 'f')
--         )
--     pure Foo {a = a, b = b}

--------------------

-- bar : Bar
-- bar =
--   { f = <| foo |>
--   , b2 = <| b f == 'F' |>
--   }

-- bar :: IO Bar
-- bar =
--   do
--     f <- delayAction $ foo
--     fb <- (f `bind` fooB) >>= \b -> b `bind` (\c -> (c == 'F'))
--     b' <- tmap (\c -> c == 'F') fb
--     pure Bar {barF = f, barFB = b'}

-------------------------------------------------------------------------------

{-
Takeaways:
- Perhaps treat all code in haskell blocks as monadic computations, in IO to
  begin with
  - Eventually allow specification of custom monads
- Values bound in a module are treated as pure, non-monadic values in other code
  blocks in the module (à la `do`)
 -}
