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
import Language.LoCo.Parser qualified
import Language.LoCo.Syntax hiding (Type)
import Language.LoCo.Syntax qualified as L

declareType :: L.Type -> Q [Dec]
declareType ty =
  case ty of
    UnsignedTy n -> pure []
    ListTy t -> pure []
    RecordTy t fields -> sequence [record id compileType t fields, record tick compileThunkedType t fields]
    ParserTy res -> pure []

record :: (Ident -> Ident) -> (L.Type -> Q Type) -> Ident -> Map Ident L.Type -> Q Dec
record rename retype recTy fieldTys = dataD (pure []) recTyName [] Nothing [ctor] []
  where
    ctor =
      recC
        recCtorName
        [ (renameField fieldName,nonStrict,) <$> retype fieldTy
          | (fieldName, fieldTy) <- Map.toList fieldTys
        ]
    recTyName = mkName (rename recTy)
    recCtorName = recTyName
    renameField = lower' . rename

nonStrict :: Bang
nonStrict = Bang NoSourceUnpackedness NoSourceStrictness

compileType :: L.Type -> Q Type
compileType ty =
  case ty of
    UnsignedTy n -> word n
    ListTy t -> appT listT (compileType t)
    RecordTy n _ -> conT (mkName n)
    ParserTy res -> parserT (compileType res)

compileThunkedType :: L.Type -> Q Type
compileThunkedType ty =
  case ty of
    UnsignedTy n -> thunkT (word n)
    ListTy t -> appT listT (compileThunkedType t)
    RecordTy n _ -> conT (tick' n)
    ParserTy res -> parserT (compileThunkedType res)

parserT :: Q Type -> Q Type
parserT = appT (conT "Parser")

regionT :: Q Type
regionT = conT "Region"

thunkT :: Q Type -> Q Type
thunkT = appT (conT "Thunk")

funT :: [Q Type] -> Q Type
funT = foldl1 (appT . appT arrowT)

word :: Int -> Q Type
word i =
  case i of
    1 -> [t|Data.Word.Word8|]
    2 -> [t|Data.Word.Word16|]
    4 -> [t|Data.Word.Word32|]
    8 -> [t|Data.Word.Word64|]
    _ -> fail ("not a supported word size: " <> show i)

-------------------------------------------------------------------------------

declareParser :: Ident -> Parser -> Q [Dec]
declareParser name Parser {..} = sequence [signature, funD name' clauses]
  where
    signature = sigD name' (funT [regionT, parserT (compileThunkedType pResult)])
    name' = mkName name

    clauses = [clause (map (varP . lower') pRegionParams) body []]
    body = normalB (compileParser pBinds pResult)

compileParser :: Map Ident Expr -> L.Type -> Q Exp
compileParser parserBinds resultTy =
  case resultTy of
    RecordTy {..} -> compileRecordParser recordName recordFieldTys parserBinds
    _ -> fail ""

-- Assume topologically sorted for now
compileRecordParser :: Ident -> Map Ident L.Type -> Map Ident Expr -> Q Exp
compileRecordParser recName recFieldTys parserBinds =
  do
    pBinds <- sequence [parseBind i e | (i, e) <- Map.toList parseResultBinds]
    rBinds <- sequence [regionBind i e | (i, e) <- Map.toList regionBinds]
    let recCon = conE (tick' recName)
        recFields = [varE (tick' recField) | recField <- Map.keys recFieldTys]
    ret <- noBindS (appE [|pure|] (foldl1 appE (recCon : recFields)))
    pure (MDoE Nothing (rBinds <> pBinds <> [ret]))
  where
    parseResultBinds = parserBinds `Map.intersection` recFieldTys
    regionBinds = parserBinds Map.\\ recFieldTys

    parseBind i e = bindS (varP (tick' i)) (expr e)
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
        L.Lit i -> litE (integerL i)
        Var v
          | Just e <- asPrim v -> e
          | v `Set.member` shouldTick -> varE (tick' v)
          | otherwise -> varE (lower' v)
        App e es -> foldl1 appE (map go (e : es))
        RegApp e r -> [|onSubRegion $(varE (lower' r)) $(go e)|]

asPrim :: Ident -> Maybe (Q Exp)
asPrim i =
  case i of
    "take" -> Just [|rTake|]
    "drop" -> Just [|rDrop|]
    "u8" -> Just [|parseU8|]
    "many" -> Just [|manyT|]
    _ -> Nothing

-------------------------------------------------------------------------------

declareEntrypoint :: Ident -> Entrypoint -> Q [Dec]
declareEntrypoint i Entrypoint {..} =
  do
    epSig <- sigD fnName [t|Language.LoCo.Parser.Parser $(compileType epTypeProjection)|]
    regionBind <- bindS [p|region|] [|topRegion|]
    parseBind <- bindS [p|result|] [|$(varE (mkName epParseBase)) region|]
    let acc = foldl (\z a -> [|$(accessor a) . $z|]) [|id|] epParseProjection
    result <- noBindS [|force ($acc result)|]
    let epFnBody = NormalB (DoE Nothing [regionBind, parseBind, result])
        epFn = FunD fnName [Clause [] epFnBody []]
    pure [epSig, epFn]
  where
    fnName = mkName i

accessor :: Accessor -> Q Exp
accessor a =
  case a of
    Field f -> varE (tick' f)
    Idx i -> [|(!! $(litE (integerL (fromIntegral i))))|]

-------------------------------------------------------------------------------

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