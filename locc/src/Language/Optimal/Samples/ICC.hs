{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -ddump-splices #-}

{-# HLINT ignore "Avoid lambda" #-}

module Language.Optimal.Samples.ICC where

import Control.Monad.IO.Class (MonadIO (..))
import Data.Bits (Bits (..))
import Data.List (foldl')
import Data.Word (Word8)
import Language.Optimal.Quote (optimal)
import Thunk.RefVal
import Thunk.Vector
import Prelude hiding (last)

data Region = Region {rBegin :: Int, rEnd :: Int}
  deriving (Show)

first :: MonadIO m => Int -> Region -> m Region
first n Region {..}
  | rBegin + n <= rEnd = pure (Region rBegin (rBegin + n))
  | otherwise = liftIO (fail "not big enough")

last :: MonadIO m => Int -> Region -> m Region
last n Region {..}
  | rEnd - n >= rBegin = pure (Region (rEnd - n) rEnd)
  | otherwise = liftIO (fail "not big enough")

between :: Int -> Int -> Region
between begin end = Region {rBegin = begin, rEnd = end}

get :: Region -> [a] -> [a]
get Region {..} = drop rBegin . take rEnd

--------------------------------------------------------------------------------

parseInt :: [Word8] -> Region -> Int
parseInt src region =
  let bytes = get region src
   in asInt bytes
  where
    asInt = foldl' shiftAdd 0
    shiftAdd int byte = int `shiftL` 8 .|. fromIntegral byte

parseHeader :: MonadIO m => Source -> Region -> m (Header m)
parseEntry :: MonadIO m => Source -> Region -> m (Entry m)
parseICC :: MonadIO m => Source -> m (ICC m)
iccHeader :: MonadIO m => Source -> Int -> m (Header m)
iccEntry :: MonadIO m => Source -> Int -> m (Entry m)

type Source = [Word8]

[optimal|
type Header = { hLen : Int, hOff : Int }

type Entry = { eBytes : Source }

type ICC = {
  iccLen : Int,
  iccHeaders : [Header],
  iccEntries : [Entry],
}

parseHeader : Source -> Region -> Header
parseHeader src region = {
  lenRegion = <| first 4 region |>,
  offRegion = <| last 4 region |>,
  hLen = <| pure (parseInt src lenRegion) |>,
  hOff = <| pure (parseInt src offRegion) |>,
}

parseEntry : Source -> Region -> Entry
parseEntry src region = {
  eBytes = <| pure (get region src) |>
}

parseICC : Source -> ICC
parseICC src = {
  lenRegion = <| pure (between 0 4) |>,
  iccLen = <| pure (parseInt src lenRegion) |>,
  
  headersRegions = generate iccLen <| \i -> pure (between (i * 8 + 4) (i * 8 + 12)) |>,
  iccHeaders = map headersRegions <| \r -> parseHeader src r |>,
  
  -- Want to deconstruct a Header in Haskell - but shouldn't, because we can't
  -- rely on Haskell as source/want to keep generated structures somewhat
  -- abstracted. Other options?
  -- - 
  iccEntries = map iccHeaders <| \h -> entryFromHeader h src |>,
}

entryFromHeader : Header -> Source -> Entry
entryFromHeader hdr src = {
  o = hdr.hOff,
  l = hdr.hLen,
  r = <| pure (between o (o + l)) |>,
  entry = module parseEntry src r,
  eBytes = entry.eBytes,
}

iccHeader : Source -> Int -> Header
iccHeader src idx = {
  icc = module parseICC src,
  headers = icc.iccHeaders,
  header = index headers idx,
  hLen = header.hLen,
  hOff = header.hOff
}

iccEntry : Source -> Int -> Entry
iccEntry src idx = {
  icc = module parseICC src,
  entries = icc.iccEntries,
  entry = index entries idx,
  eBytes = entry.eBytes,
}
|]

-- Length: 1
-- Header 0:
-- - Length: 1
-- - Offset: 12
-- Entry 0:
-- - Bytes: [42]
toyICC :: Source
toyICC = [0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 12, 42]

debugHeader :: MonadIO m => Header m -> m ()
debugHeader Header {..} =
  do
    l <- force hLen
    o <- force hOff
    liftIO $ putStrLn $ "hLen: " <> show l
    liftIO $ putStrLn $ "hOff: " <> show o

debugEntry :: MonadIO m => Entry m -> m ()
debugEntry Entry {..} =
  do
    bs <- force eBytes
    liftIO $ putStrLn $ "eBytes: " <> show bs
