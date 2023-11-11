{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Language.Optimal.Samples.Tuple where

import Control.Monad.IO.Class (MonadIO)
import Language.Optimal.Quote (optimal)
import Thunk.RefVal (Thunked, delayAction, delayTuple, force)

type Pair = (Int, Int)

mkTupleSample :: MonadIO m => m (TupleSample m)

[optimal|
type TupleSample = { pair : Pair, first : Int }

mkTupleSample : TupleSample
mkTupleSample = {
  (one, two) = <| pure (3, 7) |>,
  pair = <| pure (one, two) |>,
  first = <| pure one |>,
}
|]
