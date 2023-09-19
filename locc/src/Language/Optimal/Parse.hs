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
          uncurry VectorReplicate <$> parseVecReplicate,
          uncurry VectorGenerate <$> parseVecGenerate,
          uncurry VectorMap <$> parseVecMap,
          uncurry VectorIndex <$> parseVecIndex,
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
parseOptimalType :: Symbol -> Parser Type
parseOptimalType name = t >>= t'
  where
    t :: Parser Type
    t =
      choice
        [ List <$> bracketed go,
          Tuple <$> parenthesized (sepBy1 go (ignore (single ','))),
          Rec name <$> parseOptimalRecordType name,
          Alias <$> parseTyName
        ]
        <* ws

    t' :: Type -> Parser Type
    t' ty =
      choice
        [ ignore (chunk "->") >> Arrow ty <$> go,
          pure ty
        ]
        <* ws

    go = parseOptimalType name

    bracketed = between (ignore (single '[')) (ignore (single ']'))
    parenthesized = between (ignore (single '(')) (ignore (single ')'))

parseOptimalRecordType :: Symbol -> Parser (Env Type)
parseOptimalRecordType name = parseBindings (single ':') (parseOptimalType name)

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
parseVecReplicate :: Parser (Symbol, Exp)
parseVecReplicate =
  do
    ignore (chunk "replicate")
    len <- parseVarName
    expr <- parseValExpr
    pure (len, expr)

-- | "generate i <| e |>"
parseVecGenerate :: Parser (Symbol, Exp)
parseVecGenerate =
  do
    ignore (chunk "generate")
    len <- parseVarName
    expr <- parseValExpr
    pure (len, expr)

-- | "index xs i"
parseVecIndex :: Parser (Symbol, Symbol)
parseVecIndex =
  do
    ignore (chunk "index")
    vec <- parseVarName
    idx <- parseVarName
    pure (vec, idx)

-- | "map xs <| e |>"
parseVecMap :: Parser (Symbol, Exp)
parseVecMap =
  do
    ignore (chunk "map")
    vec <- parseVarName
    expr <- parseValExpr
    pure (vec, expr)

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
    binding = parseBinop parseVarName parseOp (const parseRhs)
    separator = single ',' >> ws

parseBinop :: Parser lhs -> Parser op -> (lhs -> Parser rhs) -> Parser (lhs, rhs)
parseBinop parseLhs parseOp parseRhs =
  do
    lhs <- parseLhs
    ws
    ignore parseOp
    rhs <- parseRhs lhs
    ws
    pure (lhs, rhs)

parseBind :: Parser lhs -> (lhs -> Parser rhs) -> Parser (lhs, rhs)
parseBind lhs = parseBinop lhs (single '=')

parseAscription :: Parser lhs -> (lhs -> Parser rhs) -> Parser (lhs, rhs)
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
    validIdentifierChar c = isAlphaNum c || c == '_' || c == '\''

parseTyName :: MonadParsec error Text m => m Symbol
parseTyName =
  do
    c <- satisfy isUpper
    cs <- many (satisfy validTyChar)
    ws
    pure (Text.pack (c : cs))
  where
    validTyChar c = isAlphaNum c || c == '_'
