{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}

{-# HLINT ignore "Unused LANGUAGE pragma" #-}

module Language.Optimal.Samples.Nested where

import Language.Optimal.Quote (optimal)
import Thunk.RefVal

[optimal|
type Foo = { fooInner : Int }

mkFoo : Foo
mkFoo = {
  fooInner = <| pure 5 |>
}

type Bar = { barInner : Int }

mkBar : Int -> Bar
mkBar i = {
  barInner = <| pure i |>
}

type Baz = { foo : Foo, bar : Bar, i : Int }

mkBaz : Baz
mkBaz = {
  foo = module mkFoo,
  x = <| pure 3 |>,
  bar = module mkBar x,
  i = bar.barInner
}

mkBaz2 : Int -> Baz
mkBaz2 x = {
  foo = module mkFoo,
  bar = module mkBar x,
  i = bar.barInner
}
|]
