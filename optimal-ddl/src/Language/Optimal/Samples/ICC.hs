{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Language.Optimal.Samples.ICC where

import Control.Monad.IO.Class (MonadIO (..))
import Daedalus.RTS.Numeric qualified as DDL
import Daedalus.RTS.Vector qualified as DDL
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import ICC
import Language.Optimal.Quote (optimal)
import Language.Optimal.Samples.Daedalus.Util
  ( Input,
    inputDrop,
    inputSlice,
    inputTake,
    newInput,
    pBEUInt32,
    runDaedalus,
  )
import System.IO.Unsafe (unsafePerformIO)
import Thunk.RefVal
import Thunk.Vector

-- Now that I've done this, what about something a little different?
--
-- Thought about applying Optimal types on top of *all* daedalus types, but that
-- would require assuming a lazy source language. We'd construct an `ICC` as
-- Daedalus understands it, then populate Optimal-declared subfields by
-- deconstructing it. But, we don't want to evince that whole Haskell value for
-- ICC, because Daedalus doesn't do anything fancy to keep that construction
-- lazy (as far as I know? TODO)

mkOptimalICC :: MonadIO m => ByteString -> m (OptimalICC m)
mkOptimalTagTable :: MonadIO m => Input -> m (OptimalTagTable m)
mkOptimalTag :: (MonadIO m, Integral a) => a -> Input -> m (OptimalTag m)
getTagTableElem :: MonadIO m => OptimalICC m -> Int -> m (OptimalTag m)

type TagTable = DDL.Vector Tag

type UInt32 = DDL.UInt 32

-- Paradigm: Optimal bindings are responsible for crafting input slices
-- (analagous to the "regions" we've been considering in, e.g., PEAR), using
-- input-manipulation functionality provided by Daedalus, which are then passed
-- to `runDaedalus`, which lifts Daedalus parsing into a monad suitable for use
-- in Optimal.

[optimal|
type OptimalICC = { profileHeader : ProfileHeader, tagTable : OptimalTagTable }

mkOptimalICC : Source -> OptimalICC
mkOptimalICC source = {
  origInput = {| newInput "<filename>" source |},

  profileHeaderInput = {| inputTake 128 origInput |},
  profileHeader      = <| parseProfileHeader profileHeaderInput |>,

  tagTableInput = {| inputDrop 128 origInput |},
  tagTable      = module mkOptimalTagTable tagTableInput,
}


type OptimalTagTable = { ttLen : UInt32, ttElems : Vec<OptimalTag> }

mkOptimalTagTable : Input -> OptimalTagTable
mkOptimalTagTable input = {
  ttLenInput = {| inputTake 4 input |},
  ttLen      = <| parseU32 ttLenInput |>,

  ttElemsInput = {| inputDrop 4 input |},
  ttElems      = generate ttLen <| \i -> mkOptimalTag i ttElemsInput |>,
}


type OptimalTag = { eSig : UInt32, eOffset : UInt32, eSize : UInt32, eElem : Tag }

mkOptimalTag : Int -> Input -> OptimalTag
mkOptimalTag idx input = {
  eHeaderSize  = {| 12 |},
  eHeaderBegin = {| fromIntegral idx * eHeaderSize |},
  eHeaderEnd   = {| eHeaderBegin + eHeaderSize |},
  eHeaderInput = {| inputSlice eHeaderBegin eHeaderEnd input |},

  eSigInput = {| inputSlice 0 4 eHeaderInput |},
  eSig      = <| parseU32 eSigInput |>,

  eOffsetInput = {| inputSlice 4 8 eHeaderInput |},
  eOffset      = <| parseU32 eOffsetInput |>,

  eSizeInput = {| inputSlice 8 12 eHeaderInput |},
  eSize      = <| parseU32 eSizeInput |>,

  -- Our `input` elides the 132 bytes that comprise the ProfileHeader and tag
  -- table length, so we adjust our offsets accordingly
  eElemBegin = {| fromIntegral eOffset - 132 |},
  eElemEnd   = {| eElemBegin + fromIntegral eSize |},
  eElemInput = {| inputSlice eElemBegin eElemEnd input |},
  eElem      = <| parseTag eSig eElemInput |>,
}


getTagTableElem : OptimalICC -> Int -> OptimalTag
getTagTableElem icc idx = {
  tagTable = icc.tagTable,
  ttElems  = tagTable.ttElems,
  ttElem   = index ttElems idx,

  eSig    = ttElem.eSig,
  eOffset = ttElem.eOffset,
  eSize   = ttElem.eSize,
  eElem   = ttElem.eElem,
}
|]

debugTagTableElem :: OptimalICC IO -> Int -> IO ()
debugTagTableElem icc idx =
  do
    OptimalTag {..} <- getTagTableElem icc idx
    force eSig >>= display "sig: "
    force eOffset >>= display "offset: "
    force eSize >>= display "size: "
    force eElem >>= display "elem: "
  where
    display msg x = putStr msg >> print x

parseProfileHeader :: MonadIO m => Input -> m ProfileHeader
parseProfileHeader = runDaedalus "parsing profile header" pProfileHeader

parseTag :: MonadIO m => UInt32 -> Input -> m Tag
parseTag sig = runDaedalus "parsing tag data" (pTag sig)

parseU32 :: MonadIO m => Input -> m UInt32
parseU32 = runDaedalus "parsing u32" pBEUInt32

-- for debugging's sake
src :: ByteString
src = unsafePerformIO (BS.readFile "sRGB_D65_colorimetric.icc")
{-# NOINLINE src #-}
