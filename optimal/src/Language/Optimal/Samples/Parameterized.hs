{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Language.Optimal.Samples.Parameterized where

import Control.Monad.IO.Class (MonadIO)
import Data.Word (Word64)
import Language.Optimal.Quote (optimal)
import Language.Optimal.Samples (facilePrimalityTest)
import Thunk.RefVal (Thunked, delayAction, force)

bar :: MonadIO m => Word64 -> m (Bar m)

[optimal|
type Foo = { foo : Int }
type Bar = { a : Bool, b : Char }

bar : Word64 -> Bar
bar prime = { 
  a = <| pure (facilePrimalityTest prime) |>,
  b = <| pure (if a then 't' else 'f') |>
}
|]
