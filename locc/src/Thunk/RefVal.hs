module Thunk.RefVal where

import Data.IORef

data ThunkVal a = Value a | Delay (IO a)

newtype Thunked a = Thunk {unThunk :: IORef (ThunkVal a)}

delayValue :: a -> IO (Thunked a)
delayValue imm =
  do
    ref <- newIORef (Value imm)
    pure (Thunk ref)

delayAction :: IO a -> IO (Thunked a)
delayAction action =
  do
    ref <- newIORef (Delay action)
    pure (Thunk ref)

force :: Thunked a -> IO a
force (Thunk ref) =
  do
    tVal <- readIORef ref
    val <- case tVal of
      Value imm -> pure imm
      Delay action -> action
    writeIORef ref (Value val)
    pure val

-- Lifted `fmap`, but allocates a new reference
tmap :: (a -> b) -> Thunked a -> IO (Thunked b)
tmap f (Thunk ref) =
  do
    tVal <- readIORef ref
    ref' <- newIORef $
      case tVal of
        Value imm -> Value (f imm)
        Delay action -> Delay (f <$> action)
    pure (Thunk ref')

-- More efficient than `tmap`
modify :: (a -> a) -> Thunked a -> IO ()
modify f (Thunk ref) = modifyIORef' ref f'
  where
    f' tVal =
      case tVal of
        Value imm -> Value (f imm)
        Delay action -> Delay (f <$> action)

bind :: Thunked a -> (a -> Thunked b) -> IO (Thunked b)
bind (Thunk ref) f =
  do
    tVal <- readIORef ref
    case tVal of
      Value imm -> pure (f imm)
      Delay action ->
        do
          ref' <- newIORef (Delay (action >>= \val -> force (f val)))
          pure (Thunk ref')

join :: Thunked (Thunked a) -> IO (Thunked a)
join (Thunk ref) =
  do
    tVal <- readIORef ref
    case tVal of
      Value imm -> pure imm
      Delay action -> delayAction (action >>= force)
