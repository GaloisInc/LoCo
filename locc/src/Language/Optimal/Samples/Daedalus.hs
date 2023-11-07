{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Language.Optimal.Samples.Daedalus where

import Control.Monad.IO.Class (MonadIO (..))
import Daedalus.RTS.Input (Input, newInput)
import Daedalus.RTS.Vector (Vector)
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.List.NonEmpty qualified as NE
import ICC
import Language.Optimal.Quote (optimal)
import RTS.ParseError (ErrorStyle (MultiError), HasSourcePaths)
import RTS.Parser (ParserG, runParser)
import RTS.ParserAPI (BasicParser (pPeek), ResultG (..))
import System.IO.Unsafe (unsafePerformIO)
import Thunk.RefVal

mkICC :: MonadIO m => ByteString -> m (OptimalICC m)

type TagTable = Vector Tag

[optimal|
type OptimalICC = { pHeader : ProfileHeader, tTable  : TagTable }

mkICC : Source -> OptimalICC
mkICC source = {
  originalInput = {| newInput "<filename>" source |},
  pHeaderAndModifiedInput = <| parseProfileHeader' originalInput |>,
  pHeader = {| fst pHeaderAndModifiedInput |},

  modifiedInput = {| snd pHeaderAndModifiedInput |},
  tTable = <| parseTagTable modifiedInput originalInput |>,
}
|]

-- | Run a daedalus parser on some input. May throw IO error on parse failure.
runDaedalus :: (MonadIO m, HasSourcePaths e, Show e) => ParserG e a -> Input -> m a
runDaedalus parser input =
  do
    liftIO (putStrLn "parsing")
    case runParser parser MultiError input of
      NoResults errs -> err (take 1000 (show errs) <> "...")
      Results xs ->
        case NE.uncons xs of
          ((x, itrace), Nothing) -> pure x
          (_, Just _) -> err "ambiguous"
  where
    err s = liftIO (fail s)

-- | A utility to produce a parse result as well as the (likely modified) parse
-- input (as in, source text) as it exists after executing that parse.
--
-- Motivation: be able to use `pTagTable` with original input as explicit
-- parameter and `pProfileHeader`-modified input as implicit* parameter
-- - *: not in the Daedalus sense, as in `?parameter`, but in the sense of the
--   input that's implicitly carried from one parser to another in a sequential
--   paradigm
--
-- Does this have the potential to introduce data dependencies where none
-- otherwise need exist?
-- - Probably not, since this input is carried from parse to parse anyway - the
--   only thing special about `pTagTable` is that it accepts an explicit stream
--   parameter at all...
--
-- Corollary: this might actually warrant tuple syntactic sugar?
runDaedalus' :: (MonadIO m, HasSourcePaths e, Show e) => ParserG e a -> Input -> m (a, Input)
runDaedalus' parser = runDaedalus parser'
  where
    -- parse, then peek
    parser' = (,) <$> parser <*> pPeek

parseProfileHeader :: MonadIO m => Input -> m ProfileHeader
parseProfileHeader = runDaedalus pProfileHeader

parseProfileHeader' :: MonadIO m => Input -> m (ProfileHeader, Input)
parseProfileHeader' = runDaedalus' pProfileHeader

parseTagTable :: MonadIO m => Input -> Input -> m TagTable
parseTagTable modifiedInput originalInput = runDaedalus (pTagTable originalInput) modifiedInput

-- It would be neat if we could have a monad that does `ParserG` actions as well
-- as IO - morally, to use `ParserG` as the Optimal monad. To accomplish this,
-- we'd need something that is like `ParserG` but happens to be `MonadIO`, and
-- maybe we'd need to be constantly lifting `ParserG` actions into this monad.

parseICC :: MonadIO m => Input -> m ICC
parseICC = runDaedalus pICC
