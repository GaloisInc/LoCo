{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Language.Optimal.Samples.Parameterized where

import Language.Optimal.Quote (optimal)
import Language.Optimal.Samples (facilePrimalityTest)
import Thunk.RefVal (Thunked, delayAction, force)

[optimal|
type Foo = { foo : Int }
type Bar = { a : Bool, b : Char }

bar : Word64 -> Bar
bar prime = { 
  a = <| pure (facilePrimalityTest prime) |>,
  b = <| pure (if a then 't' else 'f') |>
}
|]
