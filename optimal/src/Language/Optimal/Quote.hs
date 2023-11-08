module Language.Optimal.Quote where

import Data.Char (isSpace)
import Data.List (isPrefixOf)
import Data.Map qualified as Map
import Data.Text qualified as Text
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH.Quote (QuasiQuoter (..))
import Language.Haskell.TH.Syntax
import Language.Optimal.Compile (compileOptimalModuleDecls, compileOptimalTypeDecls)
import Language.Optimal.Parse (parseOptimal)
import Language.Optimal.Syntax

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
    (tyDecls, modDecls) <- either fail pure (parseOptimal parseExp (Text.pack src'))

    let tyEnv = Map.fromList [(tdName, tdType) | TypeDecl {..} <- tyDecls]

    tyQDecs <- compileOptimalTypeDecls tyEnv tyDecls
    modQDecs <- compileOptimalModuleDecls tyEnv modDecls
    pure (tyQDecs <> modQDecs)
  where
    comment = "--"
    src' = stripComments src
    stripComments =
      unlines
        . map (\l -> if startsWithComment l then "" else l)
        . lines
      where
        startsWithComment l = comment `isPrefixOf` dropWhile isSpace l
