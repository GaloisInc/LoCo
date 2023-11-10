{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Thunk.Vector where

import Control.Monad.IO.Class (MonadIO)
import Data.Vector qualified as V
import Text.Printf (printf)
import Thunk.RefVal

newtype Vector m a = Vector {vecContent :: V.Vector (Thunked m a)}

instance Show (Vector m a) where
  show Vector {..} = printf "V[%i]" (length vecContent)

vReplicate :: (Integral len, MonadIO m) => len -> m a -> m (Vector m a)
vReplicate vecLen fillAction =
  do
    vecContent <- V.replicateM (fromIntegral vecLen) (delayAction fillAction)
    pure Vector {..}

vGenerate :: (Integral len, MonadIO m) => len -> (Int -> m a) -> m (Vector m a)
vGenerate vecLen fillAction =
  do
    vecContent <- V.generateM (fromIntegral vecLen) (delayAction . fillAction)
    pure Vector {..}

-------------------------------------------------------------------------------

vIndex :: MonadIO m => Vector m a -> Int -> m a
vIndex Vector {..} idx = force (vecContent V.! idx)

--------------------------------------------------------------------------------

vMap :: MonadIO m => (a -> m b) -> Vector m a -> m (Vector m b)
vMap f Vector {..} = Vector <$> V.mapM (tmapM f) vecContent
