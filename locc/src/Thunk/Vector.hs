{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Thunk.Vector where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Vector qualified as V
import Text.Printf (printf)
import Thunk.RefVal

newtype Vector m a = Vector {vecContent :: V.Vector (Thunked m a)}

instance Show (Vector m a) where
  show Vector {..} = printf "V[%i]" (length vecContent)

vReplicate :: MonadIO m => Int -> m a -> m (Thunked m (Vector m a))
vReplicate vecLen fillAction = vec >>= delayValue
  where
    vec =
      do
        vecContent <- V.replicateM vecLen (delayAction fillAction)
        pure Vector {..}

vGenerate :: MonadIO m => Int -> (Int -> m a) -> m (Thunked m (Vector m a))
vGenerate vecLen fillAction = vec >>= delayValue
  where
    vec =
      do
        vecContent <- V.generateM vecLen (delayAction . fillAction)
        pure Vector {..}

-------------------------------------------------------------------------------

vIndex :: MonadIO m => Vector m a -> Int -> m (Thunked m a)
vIndex Vector{..} idx =
  delayAction $
    do
      force (vecContent V.! idx)

--------------------------------------------------------------------------------

vMap :: MonadIO m => (a -> m b) -> Thunked m (Vector m a) -> m (Thunked m (Vector m b))
vMap f vecThunk =
  delayAction $
    do
      Vector {..} <- force vecThunk
      Vector <$> V.mapM (tmapM f) vecContent
