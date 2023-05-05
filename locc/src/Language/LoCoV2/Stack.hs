{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.LoCoV2.Stack where

import Control.Monad.Reader
import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import GHC.TypeNats (Nat, type (+), type (-), type (<=))

-- Too clever by half?

-- data StackN (n :: Nat) a = StackN [a]
--   deriving (Show)

-- instance Foldable (StackN n) where
--   foldr f z (StackN xs) = foldr f z xs

-- instance Functor (StackN n) where
--   fmap f (StackN xs) = StackN (fmap f xs)

-- instance Traversable (StackN n) where
--   traverse f (StackN xs) = StackN <$> traverse f xs

-- emptyStackN :: StackN 0 a
-- emptyStackN = StackN []

-- pushStackN :: a -> StackN n a -> StackN (n + 1) a
-- pushStackN x (StackN s) = StackN (x : s)

-- popStackN :: 1 <= n => StackN n a -> StackN (n - 1) a
-- popStackN (StackN s) = StackN (tail s)

-- -- Can't use this in a Reader context with any sort of `local`, because the
-- -- type changes?
-- foo :: Reader (StackN n ()) Int
-- foo = local push (asks length)
--   where
--     push :: StackN n () -> StackN (n + 1) ()
--     push = pushStackN ()

--

data Stack a = Stack [a]
  deriving (Eq, Generic)

instance Hashable a => Hashable (Stack a)

pushStack :: a -> Stack a -> Stack a
pushStack x (Stack s) = Stack (x : s)

peekStack :: Stack a -> Maybe a
peekStack s = fst <$> popStack s

popStack :: Stack a -> Maybe (a, Stack a)
popStack (Stack s) =
  case s of
    [] -> Nothing
    (x : xs) -> Just (x, Stack xs)

emptyStack :: Stack a
emptyStack = Stack mempty