{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Optimal.Quote where

import Data.List (isPrefixOf)
import Data.Text qualified as Text
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Language.Optimal.Compile (compileOptimalModule, compileOptimalTypeDecl)
import Language.Optimal.Parse (fromText, parseOptimalModule, parseOptimalTypeDecl)
import Text.Megaparsec

optimal :: QuasiQuoter
optimal =
  QuasiQuoter
    { quoteDec = decls,
      quoteExp = undefined,
      quotePat = undefined,
      quoteType = undefined
    }

decls :: String -> Q [Dec]
decls src =
  do
    ds <- either fail pure parsed
    concat <$> mapM (either compileOptimalTypeDecl compileOptimalModule) ds
  where
    parsed = fromText (many (eitherP parseOptimalTypeDecl parseOptimalModule)) (Text.pack src')
    src' = stripComments "--" src

stripComments :: String -> String -> String
stripComments prefix content =
  unlines [line | line <- lines content, not (prefix `isPrefixOf` line)]

defaultDecs :: Q [Dec]
defaultDecs =
  [d|
    x :: Int
    x = 3
    |]
