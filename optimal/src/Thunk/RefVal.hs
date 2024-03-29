module Thunk.RefVal where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.IORef
import Text.Printf (printf)

data ThunkVal m a = Value a | Delay (m a)

newtype Thunked m a = Thunk {unThunk :: IORef (ThunkVal m a)}

delayValue :: MonadIO m => a -> m (Thunked m a)
delayValue imm =
  do
    ref <- liftIO (newIORef (Value imm))
    pure (Thunk ref)

delayAction :: MonadIO m => m a -> m (Thunked m a)
delayAction action =
  do
    ref <- liftIO (newIORef (Delay action))
    pure (Thunk ref)

delayTuple :: MonadIO m => m (a, b) -> m (Thunked m a, Thunked m b)
delayTuple tupleAction =
  do
    refFst <- liftIO (newIORef (Delay (fst <$> tupleAction)))
    refSnd <- liftIO (newIORef (Delay (snd <$> tupleAction)))
    pure (Thunk refFst, Thunk refSnd)

delayTuple3 :: MonadIO m => m (a, b, c) -> m (Thunked m a, Thunked m b, Thunked m c)
delayTuple3 tupleAction =
  do
    refFst <- liftIO (newIORef (Delay (one <$> tupleAction)))
    refSnd <- liftIO (newIORef (Delay (two <$> tupleAction)))
    refThd <- liftIO (newIORef (Delay (three <$> tupleAction)))
    pure (Thunk refFst, Thunk refSnd, Thunk refThd)
  where
    one (x, _, _) = x
    two (_, x, _) = x
    three (_, _, x) = x

force :: MonadIO m => Thunked m a -> m a
force (Thunk ref) =
  do
    tVal <- liftIO (readIORef ref)
    val <- case tVal of
      Value imm -> pure imm
      Delay action -> action
    liftIO (writeIORef ref (Value val))
    pure val

-- Lifted `fmap`, but allocates a new reference
tmap :: MonadIO m => (a -> b) -> Thunked m a -> m (Thunked m b)
tmap f (Thunk ref) =
  do
    tVal <- liftIO (readIORef ref)
    ref' <- liftIO $
      newIORef $
        case tVal of
          Value imm -> Value (f imm)
          Delay action -> Delay (f <$> action)
    pure (Thunk ref')

tmapM :: MonadIO m => (a -> m b) -> Thunked m a -> m (Thunked m b)
tmapM f (Thunk ref) =
  do
    tVal <- liftIO (readIORef ref)
    ref' <- liftIO $
      newIORef $
        case tVal of
          Value imm -> Delay (f imm)
          Delay action -> Delay (f =<< action)
    pure (Thunk ref')

-- More efficient than `tmap`
modify :: MonadIO m => (a -> a) -> Thunked m a -> m ()
modify f (Thunk ref) = liftIO (modifyIORef' ref f')
  where
    f' tVal =
      case tVal of
        Value imm -> Value (f imm)
        Delay action -> Delay (f <$> action)

bind :: MonadIO m => Thunked m a -> (a -> Thunked m b) -> m (Thunked m b)
bind (Thunk ref) f =
  do
    tVal <- liftIO (readIORef ref)
    case tVal of
      Value imm -> pure (f imm)
      Delay action ->
        do
          ref' <- liftIO (newIORef (Delay (action >>= \val -> force (f val))))
          pure (Thunk ref')

join :: MonadIO m => Thunked m (Thunked m a) -> m (Thunked m a)
join (Thunk ref) =
  do
    tVal <- liftIO (readIORef ref)
    case tVal of
      Value imm -> pure imm
      Delay action -> action

debug :: (MonadIO m, Show a) => Thunked m a -> m ()
debug (Thunk ref) =
  liftIO $
    do
      tv <- readIORef ref
      case tv of
        Value v -> printf "Value (%s)\n" (show v)
        Delay _ -> printf "Delay <action>\n"
