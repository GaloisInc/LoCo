module Thunk.IORefVal where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

data ThunkVal a = Value a | Delay (IO a)

instance Show a => Show (ThunkVal a) where
  show tv =
    case tv of
      Value v -> "Value " <> show v
      Delay _ -> "Delay <action>"

newtype Thunked a = Thunked {unThunked :: IO (IORef (ThunkVal a))}

-- instance MonadIO Thunked where
--   liftIO action =
--     action >>= \res -> undefined

instance Show (Thunked a) where
  show _ = "<thunk>"

-- fromThunkVal :: ThunkVal a -> IO a
-- fromThunkVal tv =
--   case tv of
--     Value v -> pure v
--     Delay a -> a

force :: Thunked a -> IO a
force (Thunked action) =
  do
    ref <- action
    thunk <- readIORef ref
    val <- case thunk of
      Value imm -> pure imm
      Delay act -> act
    writeIORef ref (Value val)
    pure val

delayValue :: a -> Thunked a
delayValue imm = Thunked (newIORef (Value imm))

delayAction :: IO a -> Thunked a
delayAction act = Thunked (newIORef (Delay act))

instance Functor Thunked where
  fmap fn (Thunked ref) =
    Thunked $
      do
        tv <- ref >>= readIORef
        case tv of
          Value v -> unThunked (delayValue (fn v))
          Delay a -> unThunked (delayAction (fn <$> a))

instance Applicative Thunked where
  pure = delayValue
  Thunked fnRef <*> Thunked valRef =
    Thunked $
      do
        thunkFn <- fnRef >>= readIORef
        thunkVal <- valRef >>= readIORef
        case (thunkFn, thunkVal) of
          (Value fImm, Value vImm) -> unThunked (delayValue (fImm vImm))
          (Value fImm, Delay vAct) -> unThunked (delayAction (fImm <$> vAct))
          (Delay fAct, Value vImm) -> unThunked (delayAction (fAct <*> pure vImm))
          (Delay fAct, Delay vAct) -> unThunked (delayAction (fAct <*> vAct))

instance Monad Thunked where
  return = pure
  Thunked valRef >>= f =
    Thunked $
      do
        val <- valRef >>= readIORef
        unThunked $
          case val of
            Value imm -> f imm
            Delay act -> delayAction $ act >>= force . f
