{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Language.LoCo.Thunk
  ( Thunk,
    app,
    app2,
    appM,
    appM2,
    force,
    thunk,
    thunkM,
  )
where

import Control.Monad.IO.Class
import Data.IORef

data ThunkVal m a
  = Value a
  | Delay (m a)

newtype Thunk m a = ThunkRef (IORef (ThunkVal m a))

thunk :: MonadIO m => a -> m (Thunk m a)
thunk v = liftIO $ ThunkRef <$> newIORef (Value v)

thunkM :: MonadIO m => m a -> m (Thunk m a)
thunkM a = liftIO $ ThunkRef <$> newIORef (Delay a)

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

app :: MonadIO m => (a -> b) -> Thunk m a -> m (Thunk m b)
app f = appM (pure . f)

app2 :: MonadIO m => (a -> b -> c) -> Thunk m a -> Thunk m b -> m (Thunk m c)
app2 f = appM2 (\a b -> pure (f a b))

appM :: MonadIO m => (a -> m b) -> Thunk m a -> m (Thunk m b)
appM f t = thunkM $
  do
    t' <- force t
    f t'

appM2 :: MonadIO m => (a -> b -> m c) -> Thunk m a -> Thunk m b -> m (Thunk m c)
appM2 f t1 t2 = thunkM $
  do
    t1' <- force t1
    t2' <- force t2
    f t1' t2'

-- newtype ThunkT m a = ThunkT {runThunkT :: m (IO a)}
--   -- deriving (Applicative, Functor, Monad, MonadIO)
--   deriving (Functor)

-- instance (Applicative m, Monad m) => Applicative (ThunkT m) where
--   pure x = ThunkT (pure (pure x))
--   ThunkT f <*> ThunkT x = ThunkT $
--     do
--       iof <- f
--       iox <- x
--       pure (iof <*> iox)

-- instance Monad m => Monad (ThunkT m) where
--   return = pure
--   x >>= f = ThunkT $
--     do
--       iox <- runThunkT x
--       let --k :: a -> IO b
--           k = runThunkT . f

--       undefined

try :: IO ()
try =
  do
    three <- thunkM verboseThree
    four <- appM verboseSucc three

    putStrLn "forcing four..."
    f <- force four
    print f

    putStrLn "forcing three..."
    t <- force three
    print t

    putStrLn "forcing three again..."
    t' <- force three
    print t'
  where
    verboseThree :: IO Int
    verboseThree = putStrLn "I yield three" >> pure 3

    verboseSucc :: Int -> IO Int
    verboseSucc i = putStrLn ("I yield succ(" <> show i <> ")") >> pure (succ i)
