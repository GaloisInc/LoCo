{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Language.Optimal.Samples.Daedalus where

import Control.Monad.IO.Class (MonadIO (..))
import Daedalus (pBEUInt32, pChunk, pSetStreamAt)
import Daedalus.RTS.Convert (convert)
import Daedalus.RTS.Input (Input, newInput)
import Daedalus.RTS.Vector qualified as DDL
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import ICC
import Language.Optimal.Quote (optimal)
import RTS (DDL, UInt, fromUInt)
import RTS.Annot (Annotation)
import RTS.ParseError (ErrorStyle (MultiError), HasSourcePaths)
import RTS.Parser (ParserG, runParser)
import RTS.ParserAPI (BasicParser (pPeek), ResultG (..))
import Thunk.RefVal
import Thunk.Vector

mkOptimalICC :: MonadIO m => ByteString -> m (OptimalICC m)
mkOptimalTagTable :: MonadIO m => ByteString -> m (OptimalTagTable m)
mkOptimalTag :: MonadIO m => Input -> m (OptimalTag m)

type TagTable = DDL.Vector Tag

type UInt32 = UInt 32

[optimal|
type OptimalICC = { pHeader : ProfileHeader, tTable  : TagTable }

mkOptimalICC : Source -> OptimalICC
mkOptimalICC source = {
  originalInput           = {| newInput "<filename>" source |},
  pHeaderAndModifiedInput = <| parseProfileHeader' originalInput |>,
  pHeader                 = {| fst pHeaderAndModifiedInput |},

  modifiedInput           = {| snd pHeaderAndModifiedInput |},
  tTable                  = <| parseTagTable modifiedInput originalInput |>,
}

type OptimalTagTable = { ttLen : UInt32, ttElems : [OptimalTag] }

type OptimalTag = { eSig : UInt32, eOffset : UInt32, eSize : UInt32, eElem : Tag }


mkOptimalTagTable : Source -> OptimalTagTable
mkOptimalTagTable source = {
  originalInput           = {| newInput "<filename>" source |},
  pHeaderAndModifiedInput = <| parseProfileHeader' originalInput |>,
  -- pHeader                 = {| fst pHeaderAndModifiedInput |},

  modifiedInput           = {| snd pHeaderAndModifiedInput |},
  ttLen                   = <| parseU32 modifiedInput |>,
  ttLenInt                = {| fromIntegral (fromUInt ttLen) |},
  ttElems                 = generate ttLenInt <| \_ -> undefined |>,
}

mkOptimalTag : Input -> OptimalTag
mkOptimalTag input = {
  eSigAndRest = <| parseU32' input |>,
  eSig = {| fst eSigAndRest |},
  eSigOutput = {| snd eSigAndRest |},

  eOffsetAndRest = <| parseU32' eSigOutput |>,
  eOffset = {| fst eOffsetAndRest |},
  eOffsetOutput = {| snd eOffsetAndRest |},

  eSizeAndRest = <| parseU32' eOffsetOutput |>,
  eSize = {| fst eSizeAndRest |},
  eSizeOutput = {| snd eSizeAndRest |},

  eElem = <| parseTag eSizeOutput eSig eOffset eSize |>,
}
|]

-- We've manually pushed Optimal "up"/"through" some Daedalus-generated values,
-- but what about doing it automatically?
--

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

parseU32 :: MonadIO m => Input -> m UInt32
parseU32 = runDaedalus pBEUInt32

parseU32' :: MonadIO m => Input -> m (UInt32, Input)
parseU32' = runDaedalus' pBEUInt32

runDaedalusAt :: (MonadIO m, DDL a) => Input -> UInt32 -> UInt32 -> ParserG Annotation a -> m a
runDaedalusAt input offset size parser = runDaedalus seekAndParse input
  where
    seekAndParse = pSetStreamAt offset' input >> pChunk size' parser
    offset' = convert offset
    size' = convert size

parseTag :: MonadIO m => Input -> UInt32 -> UInt32 -> UInt32 -> m Tag
parseTag input sig offset size = runDaedalusAt input offset size (pTag sig)

-- It would be neat if we could have a monad that does `ParserG` actions as well
-- as IO - morally, to use `ParserG` as the Optimal monad. To accomplish this,
-- we'd need something that is like `ParserG` but happens to be `MonadIO`, and
-- maybe we'd need to be constantly lifting `ParserG` actions into this monad.

parseICC :: MonadIO m => Input -> m ICC
parseICC = runDaedalus pICC
