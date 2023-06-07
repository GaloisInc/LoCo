module Thunk.IORef where

import Data.IORef (IORef, newIORef, readIORef)

newtype Thunked a = Thunked {unThunked :: IO (IORef a)}

instance Functor Thunked where
  fmap f (Thunked ref) =
    Thunked $
      do
        val <- ref >>= readIORef
        newIORef (f val)

instance Applicative Thunked where
  pure x = Thunked (newIORef x)
  Thunked funRef <*> Thunked valRef =
    Thunked $
      do
        fun <- funRef >>= readIORef
        val <- valRef >>= readIORef
        newIORef (fun val)

instance Monad Thunked where
  return = pure
  Thunked valRef >>= f =
    Thunked $
      do
        val <- valRef >>= readIORef
        unThunked (f val)

force :: Thunked a -> IO a
force (Thunked ref) = ref >>= readIORef

delay :: IO a -> Thunked a
delay action = undefined
