{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.LoCoV2.Primitives where

import Language.LoCo.Region (Region)
import Language.LoCoV2.Parser
import Language.LoCoV2.Stack
import Language.LoCoV2.Syntax.Simple

type ParserM a = ParserT IO a

-- newtype ParserM a = ParserM {unParserM :: ParserT IO a}
--   deriving
--     ( Functor,
--       Applicative,
--       Monad,
--       -- MonadIO,
--       MonadState Cache,
--       MonadError String,
--       MonadReader (Stack Context)
--     )

onRegion :: Region -> ParserM a -> ParserM a
onRegion region parser = undefined

parseU8 :: ParserM Value
parseU8 = undefined

-- How do we actually go about parsing?

-- Option: parsing maintains a "stack" of context: document name/hash, parser
-- types (e.g. u8), or other constructs (e.g. `many`), and regions. Thunks are
-- cached keyed on the hash of the context in which they're evaluated.

-- Parsing is just expression interpretation with an extra environment that maps
-- (ranges of) bytes to values. Thunked parsing is just parsing with an extra
-- constructor that asks us to check a cache before evaluating something.
-- Region-based parsing is just parsing with the added context of regions.

-- - Add the region to the context
-- - Add the parse type to the context XXX
-- - Execute the parse "locally" - i.e. not persisting the changes to the stack
-- onRegion :: Monad m => Region -> ParserT m a -> ParserT m a
-- onRegion region parser = Parser pty parse
--   where
--     parse = local push undefined
--     push = pushStack (ParseRegion r) . pushStack (ParseTy pty)

-- parseU8 :: Region -> ParserT IO Value
-- parseU8 r = Parser (Unsigned 1) action
--   where
--     action :: ParserT IO Value
--     action = undefined