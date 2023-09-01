{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.Optimal.Quote where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Text qualified as Text
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Language.Optimal.Compile (compileOptimalModuleDecl, compileOptimalTypeDecl)
import Language.Optimal.Parse (parseOptimal)
import Language.Optimal.Typecheck (expandType)

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
    (tyDecls, modDecls) <- either fail pure (parseOptimal (Text.pack src'))

    tyQDecs <- mapM compileOptimalTypeDecl tyDecls

    let typedModDecls = map (\m -> (m, expandType tyDecls m)) modDecls
    modQDecs <- mapM (uncurry compileOptimalModuleDecl) typedModDecls

    pure $ concat (tyQDecs ++ modQDecs)
  where
    comment = "--"
    src' = stripComments src
    stripComments =
      unlines
        . map (\l -> if startsWithComment l then "" else l)
        . lines
      where
        startsWithComment l = comment `isPrefixOf` dropWhile isSpace l
