module Language.LoCoEssential.Thunk where

import Control.Monad.IO.Class
import Data.IORef

data ThunkVal m a
  = Value a
  | Delay (m a)

newtype Thunk m a = ThunkRef (IORef (ThunkVal m a))

delay :: MonadIO m => m a -> m (Thunk m a)
delay a = liftIO $ ThunkRef <$> newIORef (Delay a)

force :: MonadIO m => Thunk m a -> m a
force (ThunkRef ref) =
  do
    t <- liftIO (readIORef ref)
    case t of
      Value v -> pure v
      Delay a ->
        do
          v <- a
          liftIO (writeIORef ref (Value v))
          pure v