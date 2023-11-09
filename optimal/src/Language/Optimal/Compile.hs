{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Optimal.Compile where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Haskell.TH
import Language.Haskell.TH qualified as TH
import Language.Optimal.Collection
import Language.Optimal.Compile.Haskell.Free (Free, freeVars)
import Language.Optimal.Compile.Haskell.Rename (Rename, rename)
import Language.Optimal.Compile.Haskell.TH (Haskell (asExp))
import Language.Optimal.Syntax
import Language.Optimal.Syntax qualified as Optimal
import Language.Optimal.Typecheck (checkArity, expandType)
import Language.Optimal.Util (Named (name))

-- XXX: it's possible to specialize all of this to `Exp`, or to remove the
-- `Free`/`Rename` constraints, but is it appropriate?

compileOptimalModuleDecls :: (Haskell e) => Env Optimal.Type -> [ModuleDecl e] -> Q [Dec]
compileOptimalModuleDecls tyEnv modDecls =
  do
    hsModDecls <- mapM (sequenceModuleDecl . fmap asExp) modDecls
    let elaboratedModDecls = map elaborate hsModDecls
    case mapM_ checkArity elaboratedModDecls of
      Left err -> fail err
      Right () -> pure ()
    concat <$> mapM compileOptimalModuleDecl elaboratedModDecls
  where
    elaborate ModuleDecl {..} = ModuleDecl {modTy = expandType tyEnv modTy, ..}

compileOptimalModuleDecl :: (Free e, Haskell e, Rename e) => ModuleDecl e -> Q [Dec]
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

compileModuleBindings :: (Free e, Haskell e, Rename e) => Set Name -> [(Name, ModuleBinding e)] -> Q [Stmt]
compileModuleBindings modBinds orderedModBinds =
  sequence [bindS (varP nm) (compileBinding binding) | (nm, binding) <- orderedModBinds]
  where
    compileBinding binding =
      case binding of
        Expression expr -> exprIntro modBinds expr
        Value expr -> valIntro modBinds expr
        VectorReplicate len fill -> vecReplicate modBinds len fill
        VectorGenerate len fill -> vecGenerate modBinds len fill
        VectorIndex vec idx -> vecIndex modBinds vec idx
        VectorMap vec fn -> vecMap modBinds vec fn
        ModuleIntro m params -> modIntro modBinds m params
        ModuleIndex m field -> modIndex modBinds m field

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
exprIntro :: (Free e, Haskell e, Rename e) => Set Name -> e -> Q Exp
exprIntro modBinds expr =
  [|delayAction $(forceThunks modBinds expr)|]

valIntro :: (Free e, Haskell e, Rename e) => Set Name -> e -> Q Exp
valIntro modBinds expr =
  do
    thExpr <- asExp expr
    exprIntro modBinds (AppE (VarE "pure") thExpr)

-- | Create a version of the expression that evaluates itself in a context in
-- which all its variables that refer to thunks have been forced
forceThunks :: (Free e, Haskell e, Rename e) => Set Name -> e -> Q Exp
forceThunks modBinds expr =
  do
    thunkRenaming <- mkRenaming modBinds expr
    let forceCtx = mkForceContext thunkRenaming
    let f n = fromMaybe n (thunkRenaming Map.!? n)
    let expr' = rename f expr
    thExp <- asExp expr'
    case forceCtx of
      [] -> pure thExp
      _ -> pure (DoE Nothing (forceCtx <> [NoBindS thExp]))

-- | Name refers to thunk, result has type `m a`
--
-- Corresponds to `liftIO (putStrLn "<name>") >> force <name>`
forceName :: Name -> Exp
forceName n = forceExpr (VarE n)

forcePrintName :: Name -> Exp
forcePrintName n =
  InfixE
    (Just (AppE (VarE "liftIO") (AppE (VarE "putStrLn") (LitE (StringL (show n))))))
    (VarE ">>")
    (Just (AppE (VarE "force") (VarE n)))

forceExpr :: Exp -> Exp
forceExpr =
  AppE (VarE "force")

forcePrintExpr :: String -> Exp -> Exp
forcePrintExpr s e =
  InfixE
    (Just (AppE (VarE "liftIO") (AppE (VarE "putStrLn") (LitE (StringL s)))))
    (VarE ">>")
    (Just (AppE (VarE "force") e))

-- | Create fresh names for all the thunks in an expression
--
-- NOTE: could be a Bimap, if need be
mkRenaming :: Free e => Set Name -> e -> Q (Map Name Name)
mkRenaming modBinds expr =
  let thunkVars = freeVars expr `Set.intersection` modBinds
   in sequence (Map.fromSet freshen thunkVars)

freshen :: Name -> Q Name
freshen = newName . show

mkForceContext :: Map Name Name -> [Stmt]
mkForceContext thunkRenaming =
  [stmt original fresh | (original, fresh) <- Map.toList thunkRenaming]
  where
    stmt original fresh = BindS (VarP fresh) (forceName original)

-------------------------------------------------------------------------------

-- | The result has type m (Thunked m (Vector m a))
vecReplicate :: (Free e, Haskell e, Rename e) => Set Name -> Symbol -> e -> Q Exp
vecReplicate modBinds len fill =
  do
    expr <- [|vReplicate $(varE (name len)) $(asExp fill)|]
    exprIntro modBinds expr

vecGenerate :: (Free e, Haskell e, Rename e) => Set Name -> Symbol -> e -> Q Exp
vecGenerate modBinds len fill =
  do
    expr <- [|vGenerate $(varE (name len)) $(asExp fill)|]
    exprIntro modBinds expr

-- | The result has type m (Thunked m a)
vecIndex :: Set Name -> Symbol -> Symbol -> Q Exp
vecIndex modBinds vec idx =
  do
    expr <- [|vIndex $(varE (name vec)) $(varE (name idx))|]
    exprIntro modBinds expr

vecMap :: (Free e, Haskell e, Rename e) => Set Name -> Symbol -> e -> Q Exp
vecMap modBinds vec fn =
  do
    expr <- [|vMap $(asExp fn) $(varE (name vec))|]
    exprIntro modBinds expr

--------------------------------------------------------------------------------

modIntro :: Set Name -> Symbol -> [Symbol] -> Q Exp
modIntro modBinds m ps =
  let modExpr = foldl1 AppE (map (VarE . name) (m : ps))
   in exprIntro modBinds modExpr

modIndex :: Set Name -> Symbol -> Symbol -> Q Exp
modIndex modBinds m field =
  let indexExpr = forceExpr (AppE (VarE (name field)) (VarE (name m)))
      -- We delete `f` from the module bindings because it should always
      -- refer to a record accessor, even if it happens to be previously
      -- bound in the module
      modBinds' = delete (name field) modBinds
   in exprIntro modBinds' indexExpr

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
