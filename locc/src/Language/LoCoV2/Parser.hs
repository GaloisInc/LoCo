{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# HLINT ignore "Replace case with maybe" #-}

module Language.LoCoV2.Parser where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HashMap
import Data.Hashable (Hashable)
import Data.Map (Map)
import Data.Map qualified as Map
import GHC.Generics (Generic)
import Language.LoCo.Region (Region)
import Language.LoCoV2.Stack
import Language.LoCoV2.Syntax.Simple

data Context = ParseTy ParseTy | ParseRegion Region
  deriving (Eq, Generic)

instance Hashable Context

-- A parser is an action in a monad of our design producing a value of a certain
-- type
-- data Parser m a = Parser {parserTy :: ParseTy, parserAction :: ParserT m a}

-- The parsing monad is stateful in cached evaluation, error-handling, and
-- maintaining of a stack of parsing context.
-- - XXX: what is the right ordering of this stack?
-- newtype ParserM a = ParserM
--   { runParserM :: StateT Cache' (ExceptT String (Reader (Stack Context))) a
--   }
--   deriving
--     ( Functor,
--       Applicative,
--       Monad,
--       MonadState Cache,
--       MonadError String,
--       MonadReader (Stack Context)
--     )

-- runParser :: ParserM a -> Either String a
-- runParser (ParserM p) = runReader (runExceptT (evalStateT p mempty)) emptyStack

newtype ParserT m a = ParserT
  { unParserT :: StateT Cache (ExceptT String (ReaderT (Stack Context) m)) a
  }
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadState Cache,
      MonadError String,
      MonadReader (Stack Context)
    )

instance MonadIO m => MonadIO (ParserT m) where
  liftIO a = ParserT (lift (lift (liftIO a)))

runParserT :: Monad m => ParserT m a -> m (Either String a)
runParserT (ParserT s) = runReaderT (runExceptT (evalStateT s mempty)) emptyStack

-------------------------------------------------------------------------------
-- The below represent our "entrypoint" interface?

-- | Produce a Value from the provided identifier. Note that the result may
-- require further forcing to be totally evaluated.
demand :: Monad m => Format -> Ident -> ParserT m Value
demand p i = access p i >>= force

-- | Force evaluation of a `Thunk`. Note that this is a form of WHNF evaluation,
-- and the result may require further forcing to be totally evaluated.
force :: Monad m => Thunk -> ParserT m Value
force thunk =
  do
    stack <- ask
    cache <- get
    case cache HashMap.!? (thunk, stack) of
      Just v -> pure v
      Nothing ->
        do
          v <- onExpr thunk eval
          modify (HashMap.insert (thunk, stack) v)
          pure v

-- | Select a field in a parser
access :: MonadError String m => Format -> Ident -> m Thunk
access (Format binds) bind =
  case binds Map.!? bind of
    Just thunk -> pure thunk
    Nothing -> throwError "not found"

-- | Evaluate an Expr to a sort of WHNF.
eval :: Monad m => Expr -> ParserT m Value
eval e =
  case e of
    ELit l -> pure (VLit l)

-- Preliminary: this can work for some circumstances, but doesn't differentiate
-- between expressions evaluated at different regions. Is this needed?
-- type Cache = Map Thunk Value

-- This has more promise, but how does it work with parser combinators like
-- `many`?
type Cache = HashMap (Thunk, Stack Context) Value

-------------------------------------------------------------------------------

-- We have a slew of format manipulations, including subsetting and dethunking,
-- at the conclusion of which we still have a `Format` - or a map from names to
-- thunks - that we will need to use to seed (become?) a parser.
--
-- Ask to users: if you build your monad atop ours, we'll deal with caching and
-- persistence for you?
-- - Very Haskell-specific, though...
-- - How can we accomplish this in a way that allows users to distance
--   themselves from Haskell?
-- - Hmm - perhaps output JSON?
--
-- This implies the question we haven't really answered too well: what do we
-- want users to do with this tool?



type Document = String

parse :: Format -> Ident -> Document -> Either String Value
parse format ident doc = runIdentity (runParserT (demand format ident))
