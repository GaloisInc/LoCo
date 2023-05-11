{-# LANGUAGE FlexibleContexts #-}

module Language.LoCoEssential.Driver where

import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text.IO qualified as Text
import Data.Void (Void)
import Language.LoCoEssential.Essence (LoCoModule)
import Language.LoCoEssential.Parse (parseModule)
import Text.Megaparsec (Parsec, errorBundlePretty, runParser)

fromFile ::
  (MonadIO m, MonadError IOError m) =>
  FilePath ->
  Parsec Void Text expr ->
  m (LoCoModule expr)
fromFile file parseExpr =
  do
    text <- liftIO (Text.readFile file)
    fromText (Just file) text parseExpr

fromText ::
  (MonadError IOError m) =>
  Maybe String ->
  Text ->
  Parsec Void Text expr ->
  m (LoCoModule expr)
fromText fileNameM text parseExpr =
  case runParser (parseModule parseExpr) fileName text of
    Left errBundle -> throwError (userError (errorBundlePretty errBundle))
    Right m -> pure m
  where
    fileName = fromMaybe "<string>" fileNameM