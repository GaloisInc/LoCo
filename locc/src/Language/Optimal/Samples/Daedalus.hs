{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Optimal.Samples.Daedalus where

import Control.Monad.IO.Class (MonadIO (..))
import Daedalus.RTS.Input (newInput)
import Data.ByteString (ByteString)
import Data.List.NonEmpty qualified as NE
import ICC
import Language.Optimal.Quote (optimal)
import RTS.ParseError (ErrorStyle (MultiError))
import RTS.Parser (runParser)
import RTS.ParserAPI (ResultG (..))
import Thunk.RefVal

type Source = ByteString

mkICC :: MonadIO m => ByteString -> m (OptimalICC m)

[optimal|
type OptimalICC = { icc : ICC }

mkICC : Source -> OptimalICC
mkICC src = {
  icc = <| parseICC src |>
}
|]

parseICC :: MonadIO m => ByteString -> m ICC
parseICC source =
  do
    liftIO (putStrLn "parseICC")
    let input = newInput "<source-file>" source
    case runParser pICC MultiError input of
      NoResults errs -> liftIO (fail (show errs))
      Results xs
        | length xs /= 1 ->
            liftIO (fail ("ambiguous - " <> show (length xs) <> " results"))
        | otherwise ->
            pure (fst (NE.head xs))
