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

-- The interface for this needs to be driven by the types of values we expect to
-- be able to supply to it from within *generated* code (not the types as they
-- appear in Optimal itself)
--
-- Options (cross-product of applications of `m`, `Vec`, and `Thunked`)
-- -                 a
-- -               m a
-- -         Thunked a
-- -      m (Thunked a)
-- -             Vec a
-- -          m (Vec a)
-- -    Thunked (Vec a))
-- - m (Thunked (Vec a))
--
-- Intro
-- - Length: Thunked a (a module-bound name)
-- - Length:         a (a free or parameter-bound name)
-- - Fill:         m a (an expression)
--
-- Index
-- - Vector: Thunked (Vec a) (a name)
-- - Index: Thunked a (a module-bound name)
-- - Index:         a (a free or parameter-bound name)

vReplicateThunk :: MonadIO m => Thunked m Int -> m a -> m (Thunked m (Vector m a))
vReplicateThunk lenThunk fillAction =
  delayAction $
    do
      vecLen <- force lenThunk
      vecContent <- V.replicateM vecLen (delayAction fillAction)
      pure Vector {..}

vReplicateVal :: MonadIO m => Int -> m a -> m (Thunked m (Vector m a))
vReplicateVal vecLen fillAction = vec >>= delayValue
  where
    vec =
      do
        vecContent <- V.replicateM vecLen (delayAction fillAction)
        pure Vector {..}

-------------------------------------------------------------------------------

vIndexThunk :: MonadIO m => Thunked m (Vector m a) -> Thunked m Int -> m (Thunked m a)
vIndexThunk vecThunk idxThunk =
  delayAction $
    do
      Vector {..} <- force vecThunk
      idx <- force idxThunk
      liftIO (print idx)
      force (vecContent V.! idx)

vIndexVal :: MonadIO m => Thunked m (Vector m a) -> Int -> m (Thunked m a)
vIndexVal vecThunk idx =
  delayAction $
    do
      Vector {..} <- force vecThunk
      liftIO (print idx)
      force (vecContent V.! idx)

--------------------------------------------------------------------------------

delayMap :: MonadIO m => (a -> m b) -> Thunked m (Vector m a) -> m (Thunked m (Vector m b))
delayMap f vecThunk =
  delayAction $
    do
      Vector {..} <- force vecThunk
      Vector <$> V.mapM (tmapM f) vecContent
