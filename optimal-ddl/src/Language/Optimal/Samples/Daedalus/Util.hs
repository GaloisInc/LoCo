{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-|
Helpers for creating and manipulating input streams and running Daedalus parsers
on those streams, as well as convenience instances for some Daedalus numeric
types.
-}

module Language.Optimal.Samples.Daedalus.Util
  ( Input,
    inputTake,
    inputDrop,
    inputSlice,
    newInput,
    runDaedalus,
  )
where

import Control.Monad.IO.Class (MonadIO (..))
import Daedalus.RTS.Input (Input, inputDrop, inputTake, newInput)
import Daedalus.RTS.Numeric
  ( Arith (add, div, lit, mul, sub),
    Numeric (mod),
    SizeType,
    UInt,
    fromUInt,
    toUInt,
  )
import Data.List.NonEmpty qualified as NE
import Data.Ratio ((%))
import RTS.ParseError (ErrorStyle (MultiError), HasSourcePaths)
import RTS.Parser (ParserG, runParser)
import RTS.ParserAPI (ResultG (..))
import Prelude hiding (div, mod)

-- | Run a daedalus parser on some input. Throws IO error on parse failure.
runDaedalus :: (MonadIO m, HasSourcePaths e, Show e) => String -> ParserG e a -> Input -> m a
runDaedalus msg parser input =
  do
    liftIO (putStrLn msg)
    case runParser parser MultiError input of
      NoResults errs -> err (take 1000 (show errs) <> "...")
      Results xs ->
        case NE.uncons xs of
          ((x, _inputTrace), Nothing) -> pure x
          (_, Just _) -> err "ambiguous"
  where
    err s = liftIO (fail s)

inputSlice :: UInt 64 -> UInt 64 -> Input -> Input
inputSlice begin end input = inputTake (end - begin) (inputDrop begin input)

instance SizeType n => Num (UInt n) where
  (+) = add
  (-) = sub
  (*) = mul
  abs = id
  signum 0 = 0
  signum _ = 1
  fromInteger = lit

instance SizeType n => Enum (UInt n) where
  toEnum = toUInt . fromIntegral
  fromEnum = fromIntegral . fromUInt

instance SizeType n => Real (UInt n) where
  toRational u = toInteger u % 1

instance SizeType n => Integral (UInt n) where
  toInteger = fromIntegral . fromUInt
  quotRem numerator denominator = (numerator `div` denominator, numerator `mod` denominator)
