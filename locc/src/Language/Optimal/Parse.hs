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

-- | Parse a sequence of Optimal module and type declarations.
parseOptimal :: Text -> Either String ([TypeDecl], [ModuleDecl])
parseOptimal text =
  do
    decls <- runParser (many (eitherP parseOptimalTypeDecl parseOptimalModuleDecl)) text
    pure (partitionEithers decls)

-------------------------------------------------------------------------------

parseOptimalModuleDecl :: Parser ModuleDecl
parseOptimalModuleDecl =
  do
    (modName, modTy) <- parseOptimalModuleTypeAscription
    (modParams, binds) <- parseOptimalModuleBody modName
    pure ModuleDecl {modTy = modTy, modName = modName, modParams = modParams, modEnv = binds}

parseOptimalModuleTypeAscription :: Parser (Symbol, Type)
parseOptimalModuleTypeAscription = parseBinop parseVarName (single ':') parseOptimalType

parseOptimalModuleBody :: Symbol -> Parser ([Symbol], Env (ModuleBinding Exp))
parseOptimalModuleBody modName =
  do
    ignore (chunk modName)
    params <- many parseVarName
    ignore (single '=')
    body <- parseOptimalModuleBindings
    pure (params, body)

parseOptimalModuleBindings :: Parser (Env (ModuleBinding Exp))
parseOptimalModuleBindings = parseBindings (single '=') bindingBody
  where
    bindingBody =
      choice
        [ Expression <$> parseValExpr,
          uncurry VectorReplicate <$> parseVecExpr,
          uncurry VectorIndex <$> parseIdxExpr,
          uncurry ModuleIntro <$> parseModIntro,
          uncurry ModuleIndex <$> parseModIndex
        ]

parseOptimalTypeDecl :: Parser TypeDecl
parseOptimalTypeDecl =
  do
    ignore (chunk "type")
    (name, ty) <- parseBind parseTyName parseOptimalType
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
          Rec <$> parseOptimalRecordType,
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

parseOptimalRecordType :: Parser (Env Type)
parseOptimalRecordType = parseBindings (single ':') parseOptimalType

-------------------------------------------------------------------------------

-- | "<| e |>"
parseValExpr :: Parser Exp
parseValExpr =
  do
    ignore (chunk left)
    str <- manyTill anySingle (chunk right)
    case parseExp str of
      Left err -> error $ "parse error: " <> err
      Right expr -> pure expr
  where
    (left, right) = ("<|", "|>")

-- | "replicate i <| e |>"
parseVecExpr :: Parser (Symbol, Exp)
parseVecExpr =
  do
    ignore (chunk "replicate")
    len <- parseVarName
    expr <- parseValExpr
    pure (len, expr)

-- | "index xs i"
parseIdxExpr :: Parser (Symbol, Symbol)
parseIdxExpr =
  do
    ignore (chunk "index")
    vec <- parseVarName
    idx <- parseVarName
    pure (vec, idx)

-- | "module m [p ...]"
parseModIntro :: Parser (Symbol, [Symbol])
parseModIntro =
  do
    ignore (chunk "module")
    md <- parseVarName
    params <- many parseVarName
    ws
    pure (md, params)

-- | "m.x"
parseModIndex :: Parser (Symbol, Symbol)
parseModIndex =
  do
    md <- parseVarName
    _ <- single '.'
    field <- parseVarName
    ws
    pure (md, field)

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

parseBind :: Parser lhs -> Parser rhs -> Parser (lhs, rhs)
parseBind lhs = parseBinop lhs (single '=')

parseAscription :: Parser lhs -> Parser rhs -> Parser (lhs, rhs)
parseAscription lhs = parseBinop lhs (single ':')

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
