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
import Language.Optimal.Compile.Haskell.Rename (rename)
import Language.Optimal.Syntax
import Language.Optimal.Syntax qualified as Optimal
import Language.Optimal.Typecheck (expandType)
import Language.Optimal.Util (Named (name))

compileOptimalModuleDecls :: Env Optimal.Type -> [ModuleDecl] -> Q [Dec]
compileOptimalModuleDecls tyEnv modDecls =
  concat <$> mapM (compileOptimalModuleDecl . elaborate) modDecls
  where
    elaborate ModuleDecl {..} = ModuleDecl {modTy = expandType tyEnv modTy, ..}

compileOptimalModuleDecl :: ModuleDecl -> Q [Dec]
compileOptimalModuleDecl ModuleDecl {..} =
  do
    orderedModuleBindings <- sortModuleBindings modEnv'
    assignments <- compileModuleBindings modBinds orderedModuleBindings
    (modTyName, modTyEnv) <- recordResult modTy
    mkModule <- noBindS [|pure $(constructModule modTyEnv modTyName)|]
    let body = DoE Nothing (assignments <> [mkModule])
        params = [VarP (name p) | p <- modParams]
        decl = FunD funName [Clause params (NormalB body) mempty]
    -- sigTy <- [t|forall m. MonadIO m => m $(appT (conT (name modTyName)) [t|m|])|]
    -- sig <- sigD funName (pure sigTy)
    -- pure [sig, decl]
    pure [decl]
  where
    funName = name modName
    modEnv' = Map.mapKeys name modEnv
    modBinds = Map.keysSet modEnv'

compileModuleBindings :: Set Name -> [(Name, ModuleBinding Exp)] -> Q [Stmt]
compileModuleBindings modBinds orderedModBinds =
  do
    sequence [bind nm binding | (nm, binding) <- orderedModBinds]
  where
    bind nm binding =
      case binding of
        Expression expr -> bindS (varP nm) (exprIntro modBinds expr)
        VectorReplicate (name -> len) fill
          | len `Set.member` modBinds -> bindS (varP nm) (vecIntro modBinds len fill)
          | otherwise -> bindS (varP nm) (vecIntro' modBinds len fill)
        VectorIndex (name -> vec) (name -> idx)
          | idx `Set.member` modBinds -> bindS (varP nm) (vecIndex modBinds vec idx)
          | otherwise -> bindS (varP nm) (vecIndex' modBinds vec idx)
        ModuleIntro (name -> m) (map name -> ps) ->
          let mkMod = foldl1 AppE (map VarE (m : ps))
           in bindS (varP nm) (exprIntro modBinds mkMod)
        ModuleIndex (name -> m) (name -> f) ->
          let expr = forceExpr (show m <> "." <> show f) (AppE (VarE f) (VarE m))
              -- We delete `f` from the module bindings because it should always
              -- refer to a record accessor, even if it happens to be previously
              -- bound in the module
              modBinds' = Set.delete f modBinds
           in bindS (varP nm) (exprIntro modBinds' expr)

recordResult :: MonadFail m => Optimal.Type -> m (Name, Env Optimal.Type)
recordResult modTy =
  do
    modResult <- result modTy
    case modResult of
      (Rec nm tyEnv) -> pure (name nm, tyEnv)
      _ ->
        fail $
          unlines
            [ "cannot compile module with non-record result type",
              "type: " <> show modResult
            ]
  where
    result ty =
      case ty of
        Arrow _ t2 -> result t2
        _ -> pure ty

constructModule :: Env Optimal.Type -> Name -> Q Exp
constructModule fields tyName =
  let recBinds =
        [ (modBindName, VarE modBindName)
          | modBind <- Map.keys fields,
            let modBindName = name modBind
        ]
   in pure (RecConE tyName recBinds)

--------------------------------------------------------------------------------

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
forceName :: Name -> Exp
forceName n =
  InfixE
    (Just (AppE (VarE "liftIO") (AppE (VarE "putStrLn") (LitE (StringL (show n))))))
    (VarE ">>")
    (Just (AppE (VarE "force") (VarE n)))

forceExpr :: String -> Exp -> Exp
forceExpr s e =
  InfixE
    (Just (AppE (VarE "liftIO") (AppE (VarE "putStrLn") (LitE (StringL s)))))
    (VarE ">>")
    (Just (AppE (VarE "force") e))

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
    stmt original fresh = BindS (VarP fresh) (forceName original)

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

--------------------------------------------------------------------------------

compileOptimalTypeDecls :: Env Optimal.Type -> [TypeDecl] -> Q [Dec]
compileOptimalTypeDecls tyEnv tyDecls =
  concat <$> mapM (compileOptimalTypeDecl tyEnv "m") tyDecls

compileOptimalTypeDecl :: Env Optimal.Type -> Name -> TypeDecl -> Q [Dec]
compileOptimalTypeDecl tyEnv m TypeDecl {..} =
  case ty of
    -- If we expanded to an Alias, we know it refers to a Haskell type, and so
    -- doesn't require a type synonym
    Alias alias -> (: []) <$> tySynD (name tdName) [] (go ty)
    -- Vectors require type parameters, so their declarations do too
    List t -> (: []) <$> tySynD (name tdName) [PlainTV m ()] (go ty)
    Tuple ts -> undefined
    Arrow t1 t2 -> undefined
    Rec nm fields -> compileOptimalRecordDecl tyEnv m (name nm) fields
  where
    ty = expandType tyEnv tdType
    go = compileOptimalType tyEnv m

compileOptimalRecordDecl :: Env Optimal.Type -> Name -> Name -> Env Optimal.Type -> Q [Dec]
compileOptimalRecordDecl tyEnv m recName recFields =
  do
    dec <- dataD mempty recName [PlainTV m ()] Nothing [ctor] mempty
    pure [dec]
  where
    ctor = recC recName (map (uncurry mkField) (Map.toList recFields))
    mkField fieldName optimalType =
      do
        ty <- compileThunkedOptimalType tyEnv m optimalType
        pure (name fieldName, noBang, ty)
    noBang = Bang NoSourceUnpackedness NoSourceStrictness

compileOptimalType :: Env Optimal.Type -> Name -> Optimal.Type -> Q TH.Type
compileOptimalType tyEnv m ty =
  case ty of
    Alias alias -> conT (name alias)
    List t -> appT (appT (conT "Vector") (varT m)) (go t)
    Tuple ts -> undefined
    Arrow t1 t2 -> undefined
    Rec nm fields -> appT (conT (name nm)) (varT m)
  where
    go = compileOptimalType tyEnv m

compileThunkedOptimalType :: Env Optimal.Type -> Name -> Optimal.Type -> Q TH.Type
compileThunkedOptimalType tyEnv m ty = thunked (compileOptimalType tyEnv m ty)
  where
    thunked = appT (appT (conT "Thunked") (varT m))
