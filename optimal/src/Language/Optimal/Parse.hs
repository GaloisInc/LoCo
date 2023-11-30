{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Optimal.Parse where

import Control.Applicative (Alternative)
import Control.Monad (MonadPlus, void)
import Control.Monad.Reader (MonadReader, ReaderT, ask, runReaderT)
import Data.Char (isAlphaNum, isLower, isSpace, isUpper)
import Data.Either (partitionEithers)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import Data.Void (Void)
import Language.Optimal.Syntax
import Text.Megaparsec hiding (runParser)
import Text.Megaparsec qualified as Megaparsec

newtype Parser e a = Parser {unParser :: ReaderT (String -> Either String e) (Parsec Void Text) a}
  deriving
    ( Functor,
      Applicative,
      Monad,
      Alternative,
      MonadPlus,
      MonadReader (String -> Either String e),
      MonadParsec Void Text
    )

runParser :: Parser e a -> (String -> Either String e) -> Text -> Either String a
runParser (Parser parser) parseE text =
  case Megaparsec.runParser (ws >> runReaderT parser parseE) "<stdin>" text of
    Left errBundle -> Left (errorBundlePretty errBundle)
    Right res -> pure res

-------------------------------------------------------------------------------

-- | Parse a sequence of Optimal module and type declarations.
parseOptimal :: (String -> Either String e) -> Text -> Either String ([TypeDecl], [ModuleDecl e])
parseOptimal parseE text =
  do
    decls <- runParser (many (eitherP parseOptimalTypeDecl parseOptimalModuleDecl)) parseE text
    pure (partitionEithers decls)

-------------------------------------------------------------------------------

parseOptimalModuleDecl :: Parser e (ModuleDecl e)
parseOptimalModuleDecl =
  do
    (modName, modTy) <- parseOptimalModuleTypeAscription
    (modParams, binds) <- parseOptimalModuleBody modName
    pure ModuleDecl {modTy = modTy, modName = modName, modParams = modParams, modEnv = binds}

parseOptimalModuleTypeAscription :: Parser e (Symbol, Type)
parseOptimalModuleTypeAscription = parseBinop parseVarName (single ':') parseOptimalType

parseOptimalModuleBody :: Symbol -> Parser e ([Symbol], Map (Pattern Symbol) (ModuleBinding e))
parseOptimalModuleBody modName =
  do
    ignore (chunk modName)
    params <- many parseVarName
    ignore (single '=')
    body <- parseOptimalModuleBindings
    pure (params, body)

parseOptimalModuleBindings :: Parser e (Map (Pattern Symbol) (ModuleBinding e))
parseOptimalModuleBindings = parseBindings parsePattern (single '=') parseOptimalModuleExpr

parseOptimalModuleExpr :: Parser e (ModuleBinding e)
parseOptimalModuleExpr =
  choice
    [ Expression <$> parseValExpr,
      Value <$> parsePureExpr,
      -- need `try` because `parseVecReplicate` will consume the same prefix
      -- that `parseVecReplicateLit` recognizes
      uncurry VectorReplicate <$> try parseVecReplicate,
      uncurry VectorReplicateLit <$> parseVecReplicateLit,
      uncurry VectorGenerate <$> try parseVecGenerate,
      uncurry VectorGenerateLit <$> parseVecGenerateLit,
      uncurry VectorMap <$> parseVecMap,
      uncurry VectorIndex <$> try parseVecIndex,
      uncurry VectorIndexLit <$> parseVecIndexLit,
      uncurry ModuleIntro <$> parseModIntro,
      uncurry ModuleIndex <$> parseModIndex
    ]

parseOptimalTypeDecl :: Parser e TypeDecl
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
      | Type Type
      | Type "->" Type

-->

Type -> T T'

T -> <str>
   | { <str>: Type }
   | [Type]
   | (Type,...)

T' -> "->" Type
    | Type
    | epsilon
-}
parseOptimalType :: Symbol -> Parser e Type
parseOptimalType name = t >>= t'
  where
    -- t :: Parser e Type
    t =
      choice
        [ List <$> bracketed go,
          Vec <$> (ignore (chunk "Vec") >> careted go),
          Tuple <$> parenthesized (sepBy1 go (ignore (single ','))),
          Rec name <$> parseOptimalRecordType name,
          Alias <$> parseTyName
        ]
        <* ws

    -- t' :: Type -> Parser e Type
    t' ty =
      choice
        [ ignore (chunk "->") >> Arrow ty <$> go,
          App ty <$> go,
          pure ty
        ]
        <* ws

    go = parseOptimalType name

    bracketed = between (ignore (single '[')) (ignore (single ']'))
    careted = between (ignore (single '<')) (ignore (single '>'))

parseOptimalRecordType :: Symbol -> Parser e (Env Type)
parseOptimalRecordType name = parseBindings parseVarName (single ':') (parseOptimalType name)

-------------------------------------------------------------------------------

-- runEither :: (String -> Either String a) -> String -> Parser e a
-- runEither p str =
--   case p str of
--     Left err -> error ""
--     Right r -> pure r

-- parseHSExpr :: Parser Exp
-- parseHSExpr

-- | "<| e |>"
parseValExpr :: Parser e e
parseValExpr =
  do
    ignore (chunk left)
    str <- manyTill anySingle (chunk right)
    parseE <- ask
    case parseE str of
      Left err -> error $ "parse error: " <> err
      Right expr -> pure expr
  where
    (left, right) = ("<|", "|>")

parsePureExpr :: Parser e e
parsePureExpr =
  do
    ignore (chunk left)
    str <- manyTill anySingle (chunk right)
    parseE <- ask
    case parseE str of
      Left err -> error $ "parse error: " <> err
      Right expr -> pure expr
  where
    (left, right) = ("{|", "|}")

-- | "replicate i <| e |>"
parseVecReplicate :: Parser e (Symbol, e)
parseVecReplicate =
  do
    ignore (chunk "replicate")
    len <- parseVarName
    expr <- parseValExpr
    pure (len, expr)

-- | "replicate 3 <| e |>"
parseVecReplicateLit :: Parser e (Int, e)
parseVecReplicateLit =
  do
    ignore (chunk "replicate")
    len <- parseInt
    expr <- parseValExpr
    pure (len, expr)

-- | "generate i <| e |>"
parseVecGenerate :: Parser e (Symbol, e)
parseVecGenerate =
  do
    ignore (chunk "generate")
    len <- parseVarName
    expr <- parseValExpr
    pure (len, expr)

-- | "generate 3 <| e |>"
parseVecGenerateLit :: Parser e (Int, e)
parseVecGenerateLit =
  do
    ignore (chunk "generate")
    len <- parseInt
    expr <- parseValExpr
    pure (len, expr)

-- | "index xs i"
parseVecIndex :: Parser e (Symbol, Symbol)
parseVecIndex =
  do
    ignore (chunk "index")
    vec <- parseVarName
    idx <- parseVarName
    pure (vec, idx)

-- | "index xs 3"
parseVecIndexLit :: Parser e (Symbol, Int)
parseVecIndexLit =
  do
    ignore (chunk "index")
    vec <- parseVarName
    idx <- parseInt
    pure (vec, idx)

-- | "map xs <| e |>"
parseVecMap :: Parser e (Symbol, e)
parseVecMap =
  do
    ignore (chunk "map")
    vec <- parseVarName
    expr <- parseValExpr
    pure (vec, expr)

-- | "module m [p ...]"
parseModIntro :: Parser e (Symbol, [Symbol])
parseModIntro =
  do
    ignore (chunk "module")
    md <- parseVarName
    params <- many parseVarName
    ws
    pure (md, params)

-- | "m.x"
parseModIndex :: Parser e (Symbol, Symbol)
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
parseBindings :: Ord lhs => Parser e lhs -> Parser e separator -> Parser e rhs -> Parser e (Map lhs rhs)
parseBindings parseLhs parseOp parseRhs =
  do
    binds <- braced (binding `sepEndBy1` separator)
    pure (Map.fromList binds)
  where
    binding = parseBinop parseLhs parseOp (const parseRhs)
    separator = single ',' >> ws

parseBinop :: Parser e lhs -> Parser e op -> (lhs -> Parser e rhs) -> Parser e (lhs, rhs)
parseBinop parseLhs parseOp parseRhs =
  do
    lhs <- parseLhs
    ws
    ignore parseOp
    rhs <- parseRhs lhs
    ws
    pure (lhs, rhs)

parseBind :: Parser e lhs -> (lhs -> Parser e rhs) -> Parser e (lhs, rhs)
parseBind lhs = parseBinop lhs (single '=')

parseAscription :: Parser e lhs -> (lhs -> Parser e rhs) -> Parser e (lhs, rhs)
parseAscription lhs = parseBinop lhs (single ':')

-------------------------------------------------------------------------------

parsePattern :: MonadParsec error Text m => m (Pattern Symbol)
parsePattern =
  choice
    [ Tup <$> parenthesized (sepBy1 parsePattern (ignore (single ','))),
      Sym <$> parseVarName
    ]

parseVarName :: MonadParsec error Text m => m Symbol
parseVarName =
  do
    c <- satisfy validFirstChar
    cs <- many (satisfy validIdentifierChar)
    ws
    pure (Text.pack (c : cs))
  where
    validFirstChar c = isLower c || c == '_'
    validIdentifierChar c = isAlphaNum c || c == '_' || c == '\''

parseInt :: MonadParsec error Text m => m Int
parseInt =
  do
    cs <- some (satisfy isNum)
    ws
    pure (read cs)
  where
    isNum c = c `elem` ['0' .. '9']

parseTyName :: MonadParsec error Text m => m Symbol
parseTyName =
  do
    c <- satisfy isUpper
    cs <- many (satisfy validTyChar)
    ws
    pure (Text.pack (c : cs))
  where
    validTyChar c = isAlphaNum c || c == '_'

parenthesized :: MonadParsec error Text m => m a -> m a
parenthesized p = between (ignore (single '(')) (ignore (single ')')) p <* ws

braced :: MonadParsec error Text m => m a -> m a
braced p = between (ignore (single '{')) (ignore (single '}')) p <* ws

ws :: MonadParsec error Text m => m ()
ws = void (many (satisfy isSpace))

ignore :: MonadParsec error Text m => m a -> m ()
ignore p = void p >> ws
