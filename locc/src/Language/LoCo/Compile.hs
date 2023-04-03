{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.LoCo.Compile where

import Data.Char (toLower)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.String (IsString (..))
import Data.Word
import Language.Haskell.TH
import Language.LoCo.Syntax hiding (Type)
import Language.LoCo.Syntax qualified as LC

declareType :: LC.Type -> Q [Dec]
declareType ty =
  case ty of
    UnsignedTy n -> pure []
    ListTy t -> pure []
    RecordTy ty fields -> sequence [record id id ty fields, record tick thunkT ty fields]
    ParserTy res -> pure []

record :: (Ident -> Ident) -> (Q Type -> Q Type) -> Ident -> Map Ident LC.Type -> Q Dec
record rename retype recTy fieldTys = dataD (pure []) recTyName [] Nothing [ctor] []
  where
    ctor =
      recC
        recCtorName
        [ (renameField fieldName,nonStrict,) <$> ty fieldTy
          | (fieldName, fieldTy) <- Map.toList fieldTys
        ]
    recTyName = mkName (rename recTy)
    recCtorName = recTyName
    renameField = lower' . rename
    ty = retype . compileType

nonStrict :: Bang
nonStrict = Bang NoSourceUnpackedness NoSourceStrictness

compileType :: LC.Type -> Q Type
compileType ty =
  case ty of
    UnsignedTy n -> word n
    ListTy t -> list t
    RecordTy n _ -> conT (mkName n)
    ParserTy res -> parserT (compileType res)

parserT :: Q Type -> Q Type
parserT = appT (conT "Parser")

region :: Q Type
region = conT "Region"

thunkT :: Q Type -> Q Type
thunkT = appT (conT "Thunk")

funT :: [Q Type] -> Q Type
funT = foldl1 (appT . appT arrowT)

typeAlias :: String -> Q Type -> Q Dec
typeAlias name = tySynD (mkName name) []

list :: LC.Type -> Q Type
list = appT listT . compileType

word :: Int -> Q Type
word i =
  case i of
    1 -> [t|Word8|]
    2 -> [t|Word16|]
    4 -> [t|Word32|]
    8 -> [t|Word64|]
    _ -> fail ("not a supported word size: " <> show i)

-------------------------------------------------------------------------------

declareParser :: Ident -> Parser -> Q [Dec]
declareParser name Parser {..} = sequence [signature, funD name' clauses]
  where
    signature = sigD name' (funT [region, parserT (compileType pResult)])
    name' = mkName name

    clauses = [clause (map (varP . mkName . lower) pRegionParams) body []]
    body = normalB (compileParser pBinds pResult)

compileParser :: Map Ident Expr -> LC.Type -> Q Exp
compileParser parserBinds resultTy =
  case resultTy of
    RecordTy {..} -> compileRecordParser recordName recordFieldTys parserBinds
    _ -> fail ""

-- Assume topologically sorted for now
compileRecordParser :: Ident -> Map Ident LC.Type -> Map Ident Expr -> Q Exp
compileRecordParser recName recFieldTys parserBinds =
  do
    pBinds <- sequence [parseBind i e | (i, e) <- Map.toList parseResultBinds]
    rBinds <- sequence [regionBind i e | (i, e) <- Map.toList regionBinds]
    let recCon = conE (tick' recName)
        recFields = [varE (tick' recField) | recField <- Map.keys recFieldTys]
    ret <- noBindS (appE [|pure|] (foldl1 appE (recCon : recFields)))
    pure (MDoE Nothing (pBinds <> rBinds <> [ret]))
  where
    parseResultBinds = parserBinds `Map.intersection` recFieldTys
    regionBinds = parserBinds Map.\\ recFieldTys

    parseBind i e = bindS (varP (mkName (tick i))) (expr e)
    regionBind i e = bindS (varP (lower' i)) (expr e)

    shouldTick = Map.keysSet parseResultBinds
    expr = compileExpr' shouldTick

compileExpr :: Expr -> Q Exp
compileExpr = compileExpr' mempty

compileExpr' :: Set Ident -> Expr -> Q Exp
compileExpr' shouldTick = go
  where
    go expr =
      case expr of
        LC.Lit i -> litE (integerL i)
        Var v
          | Just e <- asPrim v -> e
          | v `Set.member` shouldTick -> varE (tick' v)
          | otherwise -> varE (lower' v)
        App e es -> foldl1 appE (map go (e : es))

asPrim :: Ident -> Maybe (Q Exp)
asPrim i =
  case i of
    "take" -> Just [|rTake|]
    "drop" -> Just [|rDrop|]
    "u8" -> Just [|parseU8|]
    "many" -> Just [|manyT|]
    _ -> Nothing

instance IsString Name where
  fromString = mkName

lower :: Ident -> Ident
lower i =
  case i of
    [] -> fail "empty identifier"
    (c : cs) -> toLower c : cs

lower' :: Ident -> Name
lower' = mkName . lower

tick :: Ident -> Ident
tick i =
  case i of
    [] -> fail "empty identifier"
    _ -> i <> "'"

tick' :: Ident -> Name
tick' = mkName . tick