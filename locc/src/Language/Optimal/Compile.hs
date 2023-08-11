{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Optimal.Compile where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Haskell.TH
import Language.Haskell.TH qualified as TH
import Language.Optimal.Compile.Haskell.Free (freeVars)
import Language.Optimal.Compile.Haskell.Rename (rename)
import Language.Optimal.Syntax
import Language.Optimal.Syntax qualified as Optimal
import Language.Optimal.Typecheck (sortModuleBindings)
import Language.Optimal.Util (Named (name))

compileOptimalModuleDecl :: ModuleDecl -> Q [Dec]
compileOptimalModuleDecl ModuleDecl {..} =
  do
    orderedModuleBindings <- sortModuleBindings modEnv'
    binds <- compileModuleBindings modBinds orderedModuleBindings
    modOriginalResult <- result modOriginalTy
    modExpandedResult <- result modExpandedTy
    recordResult <-
      case (modOriginalResult, modExpandedResult) of
        (Alias alias, Rec tyEnv) -> compileRecConstr (name alias) tyEnv
        _ -> fail "cannot compile module with non-record result type"
    ret <- noBindS [|pure $(pure recordResult)|]
    let body = DoE Nothing (binds <> [ret])
        params = [VarP (name p) | p <- modParams]
        decl = FunD funName [Clause params (NormalB body) mempty]
    -- sigTy <- [t|forall m. MonadIO m => m $(appT (conT (name modTyName)) [t|m|])|]
    -- sig <- sigD funName (pure sigTy)
    -- pure [sig, decl]
    pure [decl]
  where
    funName = name modName
    modNameEnv = Map.mapKeys name modEnv
    modEnv' = Map.mapKeys name modEnv
    modBinds = Map.keysSet modNameEnv
    result ty =
      case ty of
        Arrow _ t2 -> result t2
        _ -> pure ty

compileModuleBindings :: Set Name -> [(Name, ModuleBinding Exp)] -> Q [Stmt]
compileModuleBindings modBinds orderedModBinds =
  do
    sequence [bind nm binding | (nm, binding) <- orderedModBinds]
  where
    bind nm binding =
      case binding of
        ValueBinding expr -> bindS (varP nm) (exprIntro modBinds expr)
        VectorBinding (name -> len) fill
          | len `Set.member` modBinds -> bindS (varP nm) (vecIntro modBinds len fill)
          | otherwise -> bindS (varP nm) (vecIntro' modBinds len fill)
        IndexBinding (name -> vec) (name -> idx)
          | idx `Set.member` modBinds -> bindS (varP nm) (vecIndex modBinds vec idx)
          | otherwise -> bindS (varP nm) (vecIndex' modBinds vec idx)

-- | Result has type `m (Thunked m a)`
exprIntro :: Set Name -> Exp -> Q Exp
exprIntro modBinds expr =
  [|delayAction $(forceThunks modBinds expr)|]

-- | Create a version of the expression that evaluates itself in a context in
-- which all its variables that refer to thunks have been forced
forceThunks :: Set Name -> Exp -> Q Exp
forceThunks modBinds expr =
  do
    thunkRenaming <- mkRenaming modBinds expr
    let forceCtx = mkForceContext thunkRenaming
    let f n = fromMaybe n (thunkRenaming Map.!? n)
    let expr' = rename f expr
    case forceCtx of
      [] -> pure expr'
      _ -> pure (DoE Nothing (forceCtx <> [NoBindS expr']))

-- | Name refers to thunk, result has type `m a`
--
-- Corresponds to `liftIO (putStrLn "<name>") >> force <name>`
forceExpr :: Name -> Exp
forceExpr n =
  InfixE
    (Just (AppE (VarE "liftIO") (AppE (VarE "putStrLn") (LitE (StringL (show n))))))
    (VarE ">>")
    (Just (AppE (VarE "force") (VarE n)))

-- | Create fresh names for all the thunks in an expression
--
-- NOTE: could be a Bimap, if need be
mkRenaming :: Set Name -> Exp -> Q (Map Name Name)
mkRenaming modBinds expr =
  sequence
    (Map.fromList [(n, freshen n) | n <- Set.toList (exprThunks modBinds expr)])

freshen :: Name -> Q Name
freshen = newName . show

mkForceContext :: Map Name Name -> [Stmt]
mkForceContext thunkRenaming =
  [stmt original fresh | (original, fresh) <- Map.toList thunkRenaming]
  where
    stmt original fresh = BindS (VarP fresh) (forceExpr original)

exprThunks :: Set Name -> Exp -> Set Name
exprThunks modBinds expr = freeVars expr `Set.intersection` modBinds

-------------------------------------------------------------------------------

-- | The result has type m (Thunked m (Vector m a))
--
-- The length refers to a thunk
vecIntro :: Set Name -> Name -> Exp -> Q Exp
vecIntro modBinds lenThunk fillExpr =
  do
    let fillExpr' = forceThunks modBinds fillExpr
    [|delayVec $(varE lenThunk) $fillExpr'|]

-- | The result has type m (Thunked m (Vector m a))
--
-- The length refers to a pure value
vecIntro' :: Set Name -> Name -> Exp -> Q Exp
vecIntro' modBinds lenVal fillExpr =
  do
    let fillExpr' = forceThunks modBinds fillExpr
    [|delayVec' $(varE lenVal) $fillExpr'|]

-------------------------------------------------------------------------------

-- | The result has type m (Thunked m a)
--
-- The index refers to a thunk
vecIndex :: Set Name -> Name -> Name -> Q Exp
vecIndex modBinds vecThunk idxThunk =
  [|delayIndex $(varE vecThunk) $(varE idxThunk)|]

-- | The result has type m (Thunked m a)
--
-- The index refers to a pure value
vecIndex' :: Set Name -> Name -> Name -> Q Exp
vecIndex' modBinds vecThunk idxVal =
  [|delayIndex' $(varE vecThunk) $(varE idxVal)|]

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
            let modVarName = name modVar
        ]
   in pure (RecConE tyName recBinds)

compileOptimalTypeDecl :: TypeDecl -> Q [Dec]
compileOptimalTypeDecl (TypeDecl {tdName = tdName, tdType = tdType}) =
  do
    case tdType of
      Rec recFields -> compileOptimalRecordDecl m (name tdName) recFields
      _ -> compileOptimalTySynDecl m (name tdName) tdType
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
        pure (name fieldName, noBang, thunked)
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
    Alias s -> thunked (conT (name s))
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
    Alias s -> conT (name s)
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
