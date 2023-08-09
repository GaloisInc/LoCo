{-# LANGUAGE TemplateHaskell #-}

module Language.Optimal.Compile where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH
import Language.Haskell.TH qualified as TH
import Language.LoCo.Toposort (topoSortPossibly)
import Language.Optimal.Compile.Haskell.Free (freeVars)
import Language.Optimal.Compile.Haskell.Rename (rename)
import Language.Optimal.Syntax
import Language.Optimal.Syntax qualified as Optimal

compileOptimalModuleDecl :: ModuleDecl -> Q [Dec]
compileOptimalModuleDecl ModuleDecl {..} =
  do
    orderedModuleBindings <- sortModuleBindings modEnv'
    binds <- compileModuleBindings modBinds orderedModuleBindings
    modOriginalResult <- result modOriginalTy
    modExpandedResult <- result modExpandedTy
    recordResult <-
      case (modOriginalResult, modExpandedResult) of
        (Alias alias, Rec tyEnv) -> compileRecConstr (mkName' alias) tyEnv
        _ -> fail "cannot compile module with non-record result type"
    ret <- noBindS [|pure $(pure recordResult)|]
    let body = DoE Nothing (binds <> [ret])
        params = [VarP (mkName' p) | p <- modParams]
        decl = FunD funName [Clause params (NormalB body) mempty]
    -- sigTy <- [t|forall m. MonadIO m => m $(appT (conT (mkName' modTyName)) [t|m|])|]
    -- sig <- sigD funName (pure sigTy)
    -- pure [sig, decl]
    pure [decl]
  where
    funName = mkName' modName
    modNameEnv = Map.mapKeys mkName' modEnv
    modEnv' = Map.mapKeys mkName' modEnv
    modBinds = Map.keysSet modNameEnv
    result ty =
      case ty of
        Arrow _ t2 -> result t2
        _ -> pure ty

sortModuleBindings :: MonadFail m => Map Name (ModuleBinding Exp) -> m [(Name, ModuleBinding Exp)]
sortModuleBindings modEnv =
  do
    let dependencies =
          [ (var, deps)
            | (var, binding) <- Map.toList modEnv,
              let deps = Set.toList (bindingThunks modNames binding)
          ]
    orderedNames <- reverse <$> topoSortPossibly dependencies
    pure [(name, modEnv Map.! name) | name <- orderedNames]
  where
    modNames = Map.keysSet modEnv

compileModuleBindings :: Set Name -> [(Name, ModuleBinding Exp)] -> Q [Stmt]
compileModuleBindings modBinds orderedModBinds =
  sequence [bind name binding | (name, binding) <- orderedModBinds]
  where
    bind name binding =
      case binding of
        ValueBinding expr -> bindS (varP name) (exprIntro modBinds expr)
        VectorBinding len expr -> bindS (varP name) (vecIntro modBinds (mkName' len) expr)
        IndexBinding vec expr -> bindS (varP name) (vecElim modBinds (mkName' vec) expr)

-- - Expressions will be written with free variables
-- - These variables may or may not refer to thunks
--   - If they do, they need to be forced to make the expression well-typed

-- Maybe I want a function kinda like `exprIntro'` (or maybe exactly it) that
-- can guarantee an expression's well-typedness

-- | m (Thunked m a)
exprIntro :: Set Name -> Exp -> Q Exp
exprIntro modBoundNames expr =
  [|delayAction $(forcing modBoundNames expr)|]

-- | Create a version of the expression evaluated in a context where all the
-- thunks to which it refers are forced
forcing :: Set Name -> Exp -> Q Exp
forcing modBoundNames expr =
  do
    thunkRenaming <- mkThunkRenaming modBoundNames expr
    let thunkForces = mkThunkForces thunkRenaming
        update n = fromMaybe n (thunkRenaming Map.!? n)
        expr' = rename update expr
    case thunkForces of
      [] -> pure expr'
      _ -> pure (DoE Nothing (thunkForces <> [NoBindS expr']))

exprElim :: Exp -> Exp
exprElim = undefined

-- | m a
force :: Name -> Exp
force e = InfixE (Just (AppE (VarE (mkName "liftIO")) (AppE (VarE (mkName "putStrLn")) (LitE (StringL (show e)))))) (VarE (mkName ">>")) (Just (AppE (VarE (mkName "force")) (VarE e)))

-- | m (Thunked m [Thunked m a])
vecIntro :: Set Name -> Name -> Exp -> Q Exp
vecIntro modBoundNames len fill =
  do
    let fillExpr = forcing (Set.delete len modBoundNames) fill
    [|delayVec $(varE len) $fillExpr|]

vecElim :: Set Name -> Name -> Exp -> Q Exp
vecElim modBoundNames vec idx =
  do
    let idxExpr = forcing modBoundNames idx
    [|delayIdx $(varE vec) $idxExpr|]

-- This calls for a better abstraction for vectors. They probably need to
-- package their lengths and eliminators in a nice little Haskell structure so
-- that indexing reuses previous calculation.
--
-- Or do they? Does indexing ever need to reuse previous calculations? Maybe if
-- the index is named...
--
-- We probably want to go back to the drawing board a little bit for how
-- introduction and elimination work, and what's a name and what isn't.

exprThunks :: Set Name -> Exp -> Set Name
exprThunks modBinds expr = freeVars expr `Set.intersection` modBinds

-- | What variables mentioned in this binding refer to thunks - i.e., to other
-- bindings in this module?
bindingThunks :: Set Name -> ModuleBinding Exp -> Set Name
bindingThunks modBinds binding =
  case binding of
    ValueBinding expr -> freeVars expr `Set.intersection` modBinds
    VectorBinding len expr ->
      Set.insert (mkName' len) (freeVars expr `Set.intersection` modBinds)
    IndexBinding vec expr ->
      Set.insert (mkName' vec) (freeVars expr `Set.intersection` modBinds)

-- | Create a renaming strategy for the variables in an expression that refer to
-- a binding in the surrounding module.
mkThunkRenaming :: Set Name -> Exp -> Q (Map Name Name)
mkThunkRenaming modBinds expr = mkRenaming (exprThunks modBinds expr)

mkRenaming :: Set Name -> Q (Map Name Name)
mkRenaming names = sequence (Map.fromList [(n, freshen n) | n <- Set.toList names])

-- | Create statements that bind each fresh name in the map to the `force`d
-- original
mkThunkForces :: Map Name Name -> [Stmt]
mkThunkForces thunkFreshNames =
  [stmt original fresh | (original, fresh) <- Map.toList thunkFreshNames]
  where
    stmt original fresh = BindS (VarP fresh) (force original)

freshen :: Name -> Q Name
freshen = newName . show

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
    case tdType of
      Rec recFields -> compileOptimalRecordDecl m (mkName' tdName) recFields
      _ -> compileOptimalTySynDecl m (mkName' tdName) tdType
  where
    m = mkName "m"

compileOptimalRecordDecl :: Name -> Name -> Env Optimal.Type -> Q [Dec]
compileOptimalRecordDecl monad recName recFields =
  do
    dec <- dataD context recName tyVars kind [ctor] deriv
    pure [dec]
  where
    ctor = recC ctorName ctorFields
    tyName = recName
    ctorName = tyName
    ctorFields = map (uncurry mkVarBangType) (Map.toList recFields)
    mkVarBangType fieldName optimalType =
      do
        thunked <- compileThunkedOptimalType monad optimalType
        pure (mkName' fieldName, noBang, thunked)
    context = mempty
    tyVars = [PlainTV monad ()]
    kind = Nothing
    deriv = mempty
    noBang = Bang NoSourceUnpackedness NoSourceStrictness

compileOptimalTySynDecl :: Name -> Name -> Optimal.Type -> Q [Dec]
compileOptimalTySynDecl monad name ty =
  do
    dec <- tySynD name [] (compileOptimalType monad ty)
    pure [dec]

compileThunkedOptimalType :: Name -> Optimal.Type -> Q TH.Type
compileThunkedOptimalType m oTy =
  case oTy of
    Alias s -> thunked (conT (mkName' s))
    List ty ->
      let ty' = goNonThunked ty
       in thunked (appT (appT (conT (mkName "Vector")) (varT m)) ty')
    Tuple tys -> thunked (foldl1 appT (tupleT (length tys) : map goNonThunked tys))
    Arrow t1 t2 ->
      let t1' = goNonThunked t1
          t2' = goThunked t2
       in [t|$t1' -> $t2'|]
    Rec _ -> fail "can't compile record type in non-declaration context"
  where
    goThunked = compileThunkedOptimalType m
    goNonThunked = compileOptimalType m
    thunked = appT (appT (conT (mkName "Thunked")) (varT m))

compileOptimalType :: Name -> Optimal.Type -> Q TH.Type
compileOptimalType m pty =
  case pty of
    Alias s -> conT (mkName' s)
    List ty ->
      let ty' = go ty
       in appT (conT (mkName "Vector")) ty'
    Tuple tys -> foldl1 appT (tupleT (length tys) : map go tys)
    Arrow t1 t2 ->
      let t1' = go t1
          t2' = go t2
       in [t|$t1' -> $t2'|]
    Rec _ -> fail "can't compile record type in non-declaration context"
  where
    go = compileOptimalType m

mkName' :: Text -> Name
mkName' = mkName . Text.unpack
