{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Optimal.Compile where

import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH
import Language.Optimal.Syntax

compileOptimalModule :: OptimalModule -> Q [Dec]
compileOptimalModule (OptimalModule modTy (NamedEnv modName modBinds)) = sequence [sig, decl]
  where
    sig = sigD funName [t|IO $(conT (mkName' modTy))|]
    decl = funD funName [c]
    funName = mkName' modName
    c = clause patterns (normalB body) decls
    body = doE (binds <> [ret])
    binds = [compileBind modVars var expr | (var, expr) <- Map.toList modBinds]
    ret = compileRet modTy modVars
    modVars = Map.keysSet modBinds
    patterns = mempty
    decls = mempty

compileBind :: Set Symbol -> Symbol -> Exp -> Q Stmt
compileBind modVars var expr = bindS (varP (mkName' var)) [|delayAction $(compileExpr modVars expr)|]

compileExpr :: Set Symbol -> Exp -> Q Exp
compileExpr modVars expr =
  do
    let modVarNames = Set.map mkName' modVars
        thunkVarNames = freeVars expr `Set.intersection` modVarNames
    thunkVarFreshNames <- sequence (Map.fromSet (newName . show) thunkVarNames)
    thunkBinds <-
      sequence
        [ bindS (varP fresh) [|force $(varE original)|]
          | (original, fresh) <- Map.toList thunkVarFreshNames
        ]
    let rename n = case thunkVarFreshNames Map.!? n of Just n' -> pure n'; Nothing -> pure n
    expr' <- renameExp rename expr
    case thunkBinds of
      [] -> pure expr'
      _ -> pure (DoE Nothing (thunkBinds <> [NoBindS expr']))

-- - Collect all of the free variables in this expression that are also variables
--   in the encompassing module
-- - Create a do expression that:
--   - Forces each of them
--   - Ends with the user-provided expression

compileRet :: Symbol -> Set Symbol -> Q Stmt
compileRet modTy modBinds = noBindS (appE [|pure|] (recConE (mkName' modTy) recBinds))
  where
    recBinds =
      [ pure (varName, VarE varName)
        | modVar <- Set.toList modBinds,
          let varName = mkName' modVar
      ]

compileOptimalTypeDecl :: OptimalTypeDecl -> Q [Dec]
compileOptimalTypeDecl (OptimalTypeDecl (NamedEnv recName recFields)) = sequence [decl]
  where
    decl = dataD context (mkName' recName) tyVars kind [ctor] deriv
    ctor = recC ctorName ctorFields
    tyName = mkName' recName
    ctorName = tyName
    ctorFields = map (uncurry mkVarBangType) (Map.toList recFields)
    mkVarBangType fieldName optimalType =
      do
        thunked <- appT (conT (mkName "Thunked")) (compileOptimalType optimalType)
        pure (mkName' fieldName, noBang, thunked)
    context = mempty
    tyVars = mempty
    kind = Nothing
    deriv = mempty
    noBang = Bang NoSourceUnpackedness NoSourceStrictness

compileOptimalType :: OptimalType -> Q Type
compileOptimalType pty =
  case pty of
    Bool -> [t|Bool|]
    Char -> [t|Char|]
    Ctor s -> conT (mkName' s)

mkName' :: Text -> Name
mkName' = mkName . Text.unpack

-------------------------------------------------------------------------------

class Monoid c => Collection c e | c -> e where
  member :: e -> c -> Bool
  default member :: e -> c -> Bool
  member e c = not (notMember e c)

  notMember :: e -> c -> Bool
  default notMember :: e -> c -> Bool
  notMember e c = not (member e c)

  insert :: e -> c -> c

  singleton :: e -> c
  default singleton :: e -> c
  singleton e = insert e mempty

  {-# MINIMAL (member | notMember), insert #-}

-------------------------------------------------------------------------------

newtype FreeVars = FreeVars (Set Name)
  deriving (Show)

newtype BindingVars = BindingVars (Set Name)
  deriving (Show)

instance Semigroup FreeVars where
  FreeVars f1 <> FreeVars f2 = FreeVars (f1 <> f2)

instance Semigroup BindingVars where
  BindingVars f1 <> BindingVars f2 = BindingVars (f1 <> f2)

instance Monoid FreeVars where
  mempty = FreeVars mempty

instance Monoid BindingVars where
  mempty = BindingVars mempty

instance Collection FreeVars Name where
  member e (FreeVars fvs) = Set.member e fvs
  insert e (FreeVars fvs) = FreeVars (Set.insert e fvs)

instance Collection BindingVars Name where
  member e (BindingVars bvs) = Set.member e bvs
  insert e (BindingVars bvs) = BindingVars (Set.insert e bvs)

-------------------------------------------------------------------------------

freeVars :: Exp -> Set Name
freeVars expr =
  let FreeVars fvs = expFreeVars mempty expr
   in fvs

expFreeVars :: BindingVars -> Exp -> FreeVars
expFreeVars bindings expr =
  case expr of
    VarE n
      | n `notMember` bindings -> singleton n
      | otherwise -> mempty
    ConE n -> mempty
    LitE l -> mempty
    AppE e1 e2 -> goExp e1 <> goExp e2
    AppTypeE e t -> goExp e
    InfixE e1M e2 e3M -> maybe mempty goExp e1M <> goExp e2 <> maybe mempty goExp e3M
    UInfixE e1 e2 e3 -> goExp e1 <> goExp e2 <> goExp e3
    ParensE e -> goExp e
    LamE params body -> goExp' (foldMap patBindings params) body
    -- LamCaseE matches -> undefined
    -- LamCasesE matches -> undefined
    TupE es -> foldMap goExp (catMaybes es)
    UnboxedTupE es -> foldMap goExp (catMaybes es)
    UnboxedSumE e _ _ -> goExp e
    CondE e1 e2 e3 -> goExp e1 <> goExp e2 <> goExp e3
    MultiIfE matches -> foldMap (\(guard, e) -> undefined guard <> goExp e) matches
    ListE es -> foldMap goExp es
    LetE decs e -> foldMap goDec decs <> goExp' (foldMap decBindings decs) e
    _ -> error (show expr)
  where
    goExp = expFreeVars bindings
    goExp' bindings' = expFreeVars (bindings' <> bindings)
    goDec = decFreeVars bindings

guardFreeVars :: BindingVars -> Guard -> FreeVars
guardFreeVars bindings guard =
  case guard of
    NormalG e -> expFreeVars bindings e
    PatG stmts -> foldMap (stmtFreeVars bindings) stmts

guardBindings :: Guard -> BindingVars
guardBindings guard =
  case guard of
    NormalG _ -> mempty
    PatG stmts -> foldMap stmtBindings stmts

stmtFreeVars :: BindingVars -> Stmt -> FreeVars
stmtFreeVars bindings stmt =
  case stmt of
    BindS pat e -> expFreeVars (bindings <> patBindings pat) e
    LetS decs -> foldMap (decFreeVars bindings) decs
    NoBindS e -> expFreeVars bindings e
    ParS stmtss -> foldMap (foldMap (stmtFreeVars bindings)) stmtss
    RecS stmts -> foldMap (stmtFreeVars bindings) stmts

stmtBindings :: Stmt -> BindingVars
stmtBindings stmt =
  case stmt of
    BindS pat _ -> patBindings pat
    LetS decs -> foldMap decBindings decs
    NoBindS _ -> mempty
    ParS stmtss -> foldMap (foldMap stmtBindings) stmtss
    RecS stmts -> foldMap stmtBindings stmts

decBindings :: Dec -> BindingVars
decBindings dec =
  case dec of
    ValD pat body _ -> patBindings pat
    _ -> error (show dec)

decFreeVars :: BindingVars -> Dec -> FreeVars
decFreeVars bindings dec =
  case dec of
    ValD pat (NormalB e) _ -> expFreeVars (bindings <> patBindings pat) e
    _ -> error (show dec)

patBindings :: Pat -> BindingVars
patBindings pat =
  case pat of
    LitP l -> mempty
    VarP n -> singleton n
    TupP ps -> foldMap patBindings ps
    UnboxedTupP ps -> foldMap patBindings ps
    UnboxedSumP p _ _ -> patBindings p
    ConP _ _ ps -> foldMap patBindings ps
    -- this `n` represents, e.g., the `:` in a pattern like (x:xs)
    InfixP p1 n p2 -> patBindings p1 <> singleton n <> patBindings p2
    UInfixP p1 n p2 -> patBindings p1 <> singleton n <> patBindings p2
    ParensP p -> patBindings p
    TildeP p -> patBindings p
    BangP p -> patBindings p
    AsP n p -> insert n (patBindings p)
    WildP -> mempty
    RecP recName fieldPats -> foldMap (\(n, p) -> patBindings p) fieldPats
    ListP ps -> foldMap patBindings ps
    SigP p _ -> patBindings p
    ViewP e p -> patBindings p

-------------------------------------------------------------------------------

renameExp :: (Name -> Q Name) -> Exp -> Q Exp
renameExp f expr =
  case expr of
    VarE n -> VarE <$> f n
    ConE n -> ConE <$> f n
    LitE l -> pure expr
    AppE e1 e2 -> appE (go e1) (go e2)
    ParensE e -> parensE (go e)
    CondE e1 e2 e3 -> condE (go e1) (go e2) (go e3)
    LetE decs e -> letE (map goDec decs) (go e)
    _ -> error $ "TODO: finish constructors in `renameExp` (failed on " <> take 30 (show expr) <> "...)"
  where
    -- TODO: etc

    go = renameExp f
    goDec = renameDec f

renameDec :: (Name -> Q Name) -> Dec -> Q Dec
renameDec f dec =
  case dec of
    ValD pat (NormalB e) wheres -> valD (renamePat f pat) (normalB (renameExp f e)) (map pure wheres)
    _ -> error $ "TODO: finish constructors in `renameDec` (failed on " <> take 30 (show dec) <> "...)"

renamePat :: (Name -> Q Name) -> Pat -> Q Pat
renamePat f pat =
  case pat of
    LitP l -> litP l
    VarP n -> VarP <$> f n
    _ -> error $ "TODO: finish constructors in `renamePat` (failed on " <> take 30 (show pat) <> "...)"

-- TupP ps -> foldMap patBindings ps
-- UnboxedTupP ps -> foldMap patBindings ps
-- UnboxedSumP p _ _ -> patBindings p
-- ConP _ _ ps -> foldMap patBindings ps
-- -- this `n` represents, e.g., the `:` in a pattern like (x:xs)
-- InfixP p1 n p2 -> patBindings p1 <> singleton n <> patBindings p2
-- UInfixP p1 n p2 -> patBindings p1 <> singleton n <> patBindings p2
-- ParensP p -> patBindings p
-- TildeP p -> patBindings p
-- BangP p -> patBindings p
-- AsP n p -> insert n (patBindings p)
-- WildP -> mempty
-- RecP recName fieldPats -> foldMap (\(n, p) -> patBindings p) fieldPats
-- ListP ps -> foldMap patBindings ps
-- SigP p _ -> patBindings p
-- ViewP e p -> patBindings p
