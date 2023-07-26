{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Optimal.Parse where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus)
import Data.Char (isAlphaNum, isLower, isUpper)
import Data.Either (partitionEithers)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Language.Haskell.Meta (parseExp)
import Language.Haskell.TH (Exp)
import Language.LoCoEssential.Essence ()
import Language.LoCoEssential.SimpleExpr.Parse (braced, ignore, ws)
import Language.Optimal.Syntax
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec qualified as Megaparsec

newtype Parser a = Parser {unParser :: Parsec Void Text a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadPlus,
      MonadParsec Void Text
    )

runParser :: Parser a -> Text -> Either String a
runParser (Parser parser) text =
  case Megaparsec.runParser (ws >> parser) "<stdin>" text of
    Left errBundle -> Left (errorBundlePretty errBundle)
    Right res -> pure res

-------------------------------------------------------------------------------

parseOptimal :: Text -> Either String ([TypeDecl], [ModuleDecl])
parseOptimal text =
  do
    decls <- runParser (many (eitherP parseOptimalTypeDecl parseOptimalModuleDecl)) text
    let (typeDecls, moduleDecls) = partitionEithers decls
        tyEnv = Map.fromList [(tdName, td) | td@TypeDecl {..} <- typeDecls]
    typedModuleDecls <- expandTypes tyEnv moduleDecls
    pure (typeDecls, typedModuleDecls)

expandTypes :: Env TypeDecl -> [ModuleDecl] -> Either String [ModuleDecl]
expandTypes types = mapM expandType
  where
    expandType ModuleDecl {..} =
      case types Map.!? modTyName of
        Nothing -> Left $ "couldn't find type for " <> show modName
        Just TypeDecl {..} -> Right ModuleDecl {modTy = Just tdType, ..}

-------------------------------------------------------------------------------

parseOptimalModuleDecl :: Parser ModuleDecl
parseOptimalModuleDecl =
  do
    (modName, tyName) <- parseOptimalTypeAscription
    (modName', binds) <- parseBinop (chunk modName) (single '=') (parseBindings (single '=') parseHSExpr)
    pure ModuleDecl {modTyName = tyName, modTy = Nothing, modName = modName', modEnv = binds}

parseOptimalTypeAscription :: Parser (Symbol, Symbol)
parseOptimalTypeAscription = parseBinop parseVarName (single ':') parseTyName

parseOptimalTypeDecl :: Parser TypeDecl
parseOptimalTypeDecl =
  do
    ignore (chunk "type")
    (name, ty) <- parseBinop parseTyName (single '=') parseOptimalType
    pure TypeDecl {tdName = name, tdType = ty}

{-
Type -> <str>
      | { <str>: Type }
      | [Type]
      | (Type,...)
      | Type "->" Type

-->

Type -> T T'

T -> <str>
   | { <str>: Type }
   | [Type]
   | (Type,...)

T' -> "->" Type
    | epsilon
-}
parseOptimalType :: Parser Type
parseOptimalType = t >>= t'
  where
    t :: Parser Type
    t =
      choice
        [ List <$> bracketed parseOptimalType,
          Tuple <$> parenthesized (sepBy1 parseOptimalType (ignore (single ','))),
          Rec <$> parseBindings (single ':') parseOptimalType,
          Alias <$> parseTyName
        ]
        <* ws

    t' :: Type -> Parser Type
    t' ty =
      choice
        [ ignore (chunk "->") >> Arrow ty <$> parseOptimalType,
          pure ty
        ]
        <* ws

    bracketed = between (ignore (single '[')) (ignore (single ']'))
    parenthesized = between (ignore (single '(')) (ignore (single ')'))

-------------------------------------------------------------------------------

parseHSExpr :: Parser Exp
parseHSExpr =
  do
    ignore (chunk left)
    str <- manyTill anySingle (chunk right)
    case parseExp str of
      Left err -> error $ "parse error: " <> err
      Right expr -> pure expr
  where
    (left, right) = ("<|", "|>")

-------------------------------------------------------------------------------

-- | Parse a curly-braced, comma-separated, non-empty set of "bindings",
-- producing a mapping from symbols to expressions. Parsing is parametric over
-- binding syntax ('=', e.g.) and expression syntax (e.g. `Exp`)
parseBindings :: Parser separator -> Parser rhs -> Parser (Env rhs)
parseBindings parseOp parseRhs =
  do
    binds <- braced (binding `sepBy1` separator)
    pure (Map.fromList binds)
  where
    binding = parseBinop parseVarName parseOp parseRhs
    separator = single ',' >> ws

parseBinop :: Parser lhs -> Parser op -> Parser rhs -> Parser (lhs, rhs)
parseBinop parseLhs parseOp parseRhs =
  do
    lhs <- parseLhs
    ws
    ignore parseOp
    rhs <- parseRhs
    ws
    pure (lhs, rhs)

-------------------------------------------------------------------------------

parseVarName :: MonadParsec error Text m => m Symbol
parseVarName =
  do
    c <- satisfy isLower
    cs <- many (satisfy validIdentifierChar)
    ws
    pure (Text.pack (c : cs))
  where
    validIdentifierChar c = isAlphaNum c || c == '_'

parseTyName :: MonadParsec error Text m => m Symbol
parseTyName =
  do
    c <- satisfy isUpper
    cs <- many (satisfy validTyChar)
    ws
    pure (Text.pack (c : cs))
  where
    validTyChar c = isAlphaNum c || c == '_'
