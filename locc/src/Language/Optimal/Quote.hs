{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Optimal.Quote where

import Data.List (isPrefixOf)
import Data.Text qualified as Text
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Language.Optimal.Compile (compileOptimalModuleDecl, compileOptimalTypeDecl)
import Language.Optimal.Parse (parseOptimal')

optimal :: QuasiQuoter
optimal =
  QuasiQuoter
    { quoteDec = decls,
      quoteExp = \_ -> fail "cannot use `optimal` in expression contexts",
      quotePat = \_ -> fail "cannot use `optimal` in pattern contexts",
      quoteType = \_ -> fail "cannot use `optimal` in type contexts"
    }

decls :: String -> Q [Dec]
decls src =
  do
    (tyDecls, modDecls) <- either fail pure (parseOptimal' (Text.pack src'))
    tyQDecs <- mapM compileOptimalTypeDecl tyDecls
    modQDecs <- mapM compileOptimalModuleDecl modDecls
    pure $ concat (tyQDecs ++ modQDecs)
  where
    src' = stripComments "--" src

stripComments :: String -> String -> String
stripComments prefix = unlines . filter (not . (prefix `isPrefixOf`)) . lines
