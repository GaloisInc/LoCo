{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Optimal.Compile where

import Control.Monad.IO.Class
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH
import Language.Haskell.TH qualified as TH
import Language.LoCo.Toposort (topoSortPossibly)
import Language.Optimal.Syntax
import Language.Optimal.Syntax qualified as Optimal

compileOptimalModuleDecl :: ModuleDecl -> Q [Dec]
compileOptimalModuleDecl ModuleDecl {..} =
  do
    orderedModNames <-
      reverse
        <$> topoSortPossibly
          [ (mkName' var, fvs)
            | (var, expr) <- Map.toList modEnv,
              let fvs = Set.toList (freeVars expr `Set.intersection` modBinds)
          ]
    binds <- sequence [compileBind modBinds name (modNameEnv Map.! name) | name <- orderedModNames]
    recConstr <- case modTy of
      Just (Rec tyEnv) -> compileRecConstr (mkName' modTyName) tyEnv
      Just _ -> fail ""
      Nothing -> fail ""
    result <- noBindS [|pure $(pure recConstr)|]
    let body = DoE Nothing (binds <> [result])
        decl = FunD funName [Clause mempty (NormalB body) mempty]
    sigTy <- [t|forall m. MonadIO m => m $(appT (conT (mkName' modTyName)) [t|m|])|]
    sig <- sigD funName (pure sigTy)
    pure [sig, decl]
  where
    funName = mkName' modName
    modNameEnv = Map.mapKeys mkName' modEnv
    modBinds = Map.keysSet modNameEnv

compileBind :: Set Name -> Name -> Exp -> Q Stmt
compileBind modBindings name expr =
  bindS (varP name) [|delayAction $(compileExpr modBindings expr)|]

-- - Collect all of the free variables in this expression that are also variables
--   in the encompassing module
-- - Create a do expression that:
--   - Forces each of them
--   - Ends with the user-provided expression
compileExpr :: Set Name -> Exp -> Q Exp
compileExpr modBinds expr =
  do
    let thunkVarNames = freeVars expr `Set.intersection` modBinds
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

compileRet :: Name -> Set Name -> Q Stmt
compileRet modTyName modBinds = noBindS [|pure $(recConE modTyName recBinds)|]
  where
    recBinds :: [Q FieldExp]
    recBinds =
      [ pure (modVarName, VarE modVarName)
        | modVarName <- Set.toList modBinds
      ]

compileRecConstr :: Name -> Env Optimal.Type -> Q Exp
compileRecConstr tyName tyEnv =
  let recBinds =
        [ (modVarName, VarE modVarName)
          | modVar <- Map.keys tyEnv,
            let modVarName = mkName' modVar
        ]
   in pure (RecConE tyName recBinds)

compileOptimalTypeDecl :: TypeDecl -> Q [Dec]
compileOptimalTypeDecl (TypeDecl {tdName = tdName, tdType = tdType}) =
  do
    dec <-
      case tdType of
        Rec recFields -> compileOptimalRecordDecl (mkName' tdName) recFields
        Alias ty -> compileOptimalTypeSynDecl (mkName' tdName) (mkName' ty)
    pure [dec]

compileOptimalRecordDecl :: Name -> Env Optimal.Type -> Q Dec
compileOptimalRecordDecl recName recFields = decl
  where
    decl = dataD context recName tyVars kind [ctor] deriv
    ctor = recC ctorName ctorFields
    tyName = recName
    ctorName = tyName
    ctorFields = map (uncurry mkVarBangType) (Map.toList recFields)
    mkVarBangType fieldName optimalType =
      do
        thunked <- appT (appT (conT (mkName "Thunked")) (varT m)) (compileOptimalType optimalType)
        pure (mkName' fieldName, noBang, thunked)
    context = mempty
    tyVars = [PlainTV m ()]
    m = mkName "m"
    kind = Nothing
    deriv = mempty
    noBang = Bang NoSourceUnpackedness NoSourceStrictness

compileOptimalTypeSynDecl :: Name -> Name -> Q Dec
compileOptimalTypeSynDecl tdName tdAlias = tySynD tdName [] (varT tdAlias)

compileOptimalType :: Optimal.Type -> Q TH.Type
compileOptimalType pty =
  case pty of
    Bool -> [t|Bool|]
    Char -> [t|Char|]
    List ty -> [t|[$(compileOptimalType ty)]|]
    Alias s -> conT (mkName' s)

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
    CaseE e matches -> goExp e <> foldMap goMatch matches
    _ -> error $ "TODO: finish constructors in `expFreeVars` (failed on " <> take 30 (show expr) <> "...)"
  where
    goExp = expFreeVars bindings
    goExp' bindings' = expFreeVars (bindings' <> bindings)
    goDec = decFreeVars bindings
    goMatch = matchFreeVars bindings

matchFreeVars :: BindingVars -> Match -> FreeVars
matchFreeVars bindings mtch =
  case mtch of
    Match pat bod decs ->
      let bindings' = patBindings pat <> foldMap decBindings decs <> bindings
       in bodyFreeVars bindings' bod

bodyFreeVars :: BindingVars -> Body -> FreeVars
bodyFreeVars bindings bod =
  case bod of
    NormalB e -> expFreeVars bindings e
    GuardedB gs ->
      let (guards, exps) = unzip gs
       in foldMap (guardFreeVars bindings) guards <> foldMap (expFreeVars bindings) exps

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
    UInfixE e1 e2 e3 -> uInfixE (go e1) (go e2) (go e3)
    ListE es -> listE (map go es)
    CaseE e matches -> caseE (go e) (map goMatch matches)
    _ -> error $ "TODO: finish constructors in `renameExp` (failed on " <> take 30 (show expr) <> "...)"
  where
    -- TODO: etc

    go = renameExp f
    goDec = renameDec f
    goMatch = renameMatch f

renameMatch :: (Name -> Q Name) -> Match -> Q Match
renameMatch f m =
  case m of
    Match pat (NormalB expr) decs -> 
      match (renamePat f pat) (normalB (renameExp f expr)) (map (renameDec f) decs)
    _ -> error $ "TODO: finish constructors in `renameMatch` (failed on " <> take 30 (show m) <> "...)"

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
    ConP ctor tyArgs patArgs ->
      ConP
        <$> f ctor
        <*> pure tyArgs -- shouldn't need to rename types...
        <*> mapM (renamePat f) patArgs
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
