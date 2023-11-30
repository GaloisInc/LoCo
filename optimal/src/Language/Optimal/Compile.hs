{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Language.Optimal.Compile where

import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.Trans (lift)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Haskell.TH
import Language.Haskell.TH qualified as TH
import Language.Optimal.Collection
import Language.Optimal.Compile.Free (Free, freeVars)
import Language.Optimal.Compile.Haskell (Haskell (asExp))
import Language.Optimal.Compile.Rename (Rename, rename)
import Language.Optimal.Syntax
import Language.Optimal.Syntax qualified as Optimal
import Language.Optimal.Typecheck (checkArity, expandType)
import Language.Optimal.Util (Named (name))

-- XXX: it's possible to specialize all of this to `Exp`, or to remove the
-- `Free`/`Rename` constraints, but is it appropriate?

compileOptimalModuleDecls :: (Haskell e) => Bool -> Env Optimal.Type -> [ModuleDecl e] -> Q [Dec]
compileOptimalModuleDecls verbose tyEnv modDecls =
  do
    hsModDecls <- mapM (sequenceModuleDecl . fmap asExp) modDecls
    let elaboratedModDecls = map elaborate hsModDecls
    case mapM_ checkArity elaboratedModDecls of
      Left err -> fail err
      Right () -> pure ()
    concat <$> mapM (compileOptimalModuleDecl verbose) elaboratedModDecls
  where
    elaborate ModuleDecl {..} = ModuleDecl {modTy = expandType tyEnv modTy, ..}

compileOptimalModuleDecl :: (Free e, Haskell e, Rename e) => Bool -> ModuleDecl e -> Q [Dec]
compileOptimalModuleDecl verbose ModuleDecl {..} =
  do
    orderedModuleBindings <- sortModuleBindings modEnv
    let mqEnv = mkModQEnv modNameBinds (name modName) verbose
    assignments <- runModQ (compileModuleBindings orderedModuleBindings) mqEnv
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
    modSymBinds = concatMap patSyms (Map.keys modEnv)
    modNameBinds = Set.fromList (map name modSymBinds)

data ModQEnv = ModQEnv
  { mcModBinds :: Set Name,
    mcPath :: [Name], -- [Symbol]?
    mcVerbose :: Bool
  }

mkModQEnv :: Set Name -> Name -> Bool -> ModQEnv
mkModQEnv mcModBinds mcMod mcVerbose = ModQEnv {..}
  where
    mcPath = [mcMod]

showPath :: [Name] -> String
showPath names = intercalate "." (map show (reverse names))

newtype ModQ a = ModQ {unModQ :: ReaderT ModQEnv Q a}
  deriving (Applicative, Functor, Monad, MonadReader ModQEnv)

runModQ :: ModQ a -> ModQEnv -> Q a
runModQ (ModQ mc) = runReaderT mc

onPath :: Name -> ModQ a -> ModQ a
onPath n = local (\ModQEnv {..} -> ModQEnv {mcPath = n : mcPath, ..})

withBinds :: Set Name -> ModQ a -> ModQ a
withBinds binds = local (\ModQEnv {..} -> ModQEnv {mcModBinds = binds, ..})

q :: Q a -> ModQ a
q action = ModQ (lift action)

getBinds :: ModQ (Set Name)
getBinds = asks mcModBinds

instance Quote ModQ where
  newName n = ModQ (lift (newName n))

instance MonadFail ModQ where
  fail = ModQ . lift . fail

compileModuleBindings :: (Free e, Haskell e, Rename e) => [(Pattern Name, ModuleBinding e)] -> ModQ [Stmt]
compileModuleBindings orderedModBinds =
  sequence [bindS (mkPat pat) (compileBinding pat binding) | (pat, binding) <- orderedModBinds]
  where
    mkPat pat =
      case pat of
        Sym s -> varP s
        Tup ss -> tupP (map mkPat ss)

    compileBinding pat binding =
      case (pat, binding) of
        (Sym s, Expression expr) -> onPath s $ exprIntro expr
        (Tup ss, Expression expr) -> onPath undefined $ tupleIntro (length ss) expr
        (Tup _, _) -> fail "cannot bind a tuple to a non-expression"
        (Sym s, Value expr) -> onPath s $ valIntro expr
        (Sym s, VectorReplicate len fill) -> onPath s $ vecReplicate len fill
        (Sym s, VectorGenerate len fill) -> onPath s $ vecGenerate len fill
        (Sym s, VectorGenerateLit len fill) -> onPath s $ vecGenerateLit len fill
        (Sym s, VectorIndex vec idx) -> onPath s $ vecIndex vec idx
        (Sym s, VectorIndexLit vec idx) -> onPath s $ vecIndexLit vec idx
        (Sym s, VectorMap vec fn) -> onPath s $ vecMap vec fn
        (Sym s, ModuleIntro m params) -> onPath s $ modIntro m params
        (Sym _, ModuleIndex m field) -> onPath (name m) $ onPath (name field) $ modIndex m field

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
exprIntro :: (Free e, Haskell e, Rename e) => e -> ModQ Exp
exprIntro expr =
  do
    expr' <- asExp expr
    verbose <- asks mcVerbose
    path <- asks mcPath
    let expr''
          | verbose =
              UInfixE
                (AppE (VarE "liftIO") (AppE (VarE "putStrLn") (LitE (StringL (showPath path)))))
                (VarE ">>")
                expr'
          | otherwise = expr'

    [|delayAction $(forceThunks expr'')|]

-- | Result has type `m (Thunked m a, Thunked m b)`
tupleIntro :: (Free e, Haskell e, Rename e) => Int -> e -> ModQ Exp
tupleIntro len expr
  | len == 2 = [|delayTuple $(forceThunks expr)|]
  | len == 3 = [|delayTuple3 $(forceThunks expr)|]
  | otherwise = fail $ "A " <> show len <> "-tuple is too large for Optimal"

valIntro :: (Free e, Haskell e, Rename e) => e -> ModQ Exp
valIntro expr =
  do
    thExpr <- asExp expr
    exprIntro (AppE (VarE "pure") thExpr)

-- | Create a version of the expression that evaluates itself in a context in
-- which all its variables that refer to thunks have been forced
forceThunks :: (Free e, Haskell e, Rename e) => e -> ModQ Exp
forceThunks expr =
  do
    modBinds <- getBinds
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
mkRenaming :: Free e => Set Name -> e -> ModQ (Map Name Name)
mkRenaming modBinds expr =
  let thunkVars = freeVars expr `Set.intersection` modBinds
   in sequence (Map.fromSet freshen thunkVars)

freshen :: Name -> ModQ Name
freshen = newName . show

mkForceContext :: Map Name Name -> [Stmt]
mkForceContext thunkRenaming =
  [stmt original fresh | (original, fresh) <- Map.toList thunkRenaming]
  where
    stmt original fresh = BindS (VarP fresh) (forceName original)

-------------------------------------------------------------------------------

-- | The result has type m (Thunked m (Vector m a))
vecReplicate :: (Free e, Haskell e, Rename e) => Symbol -> e -> ModQ Exp
vecReplicate len fill =
  do
    expr <- [|vReplicate $(varE (name len)) $(asExp fill)|]
    exprIntro expr

vecGenerate :: (Free e, Haskell e, Rename e) => Symbol -> e -> ModQ Exp
vecGenerate len fill =
  do
    expr <- [|vGenerate $(varE (name len)) $(asExp fill)|]
    exprIntro expr

vecGenerateLit :: (Free e, Haskell e, Rename e) => Int -> e -> ModQ Exp
vecGenerateLit len fill =
  do
    expr <- [|vGenerate $(litE (integerL (fromIntegral len))) $(asExp fill)|]
    exprIntro expr

-- | The result has type m (Thunked m a)
vecIndex :: Symbol -> Symbol -> ModQ Exp
vecIndex vec idx =
  do
    expr <- [|vIndex $(varE (name vec)) $(varE (name idx))|]
    exprIntro expr

vecIndexLit :: Symbol -> Int -> ModQ Exp
vecIndexLit vec idx =
  do
    expr <- [|vIndex $(varE (name vec)) $(litE (integerL (fromIntegral idx)))|]
    exprIntro expr

vecMap :: (Free e, Haskell e, Rename e) => Symbol -> e -> ModQ Exp
vecMap vec fn =
  do
    expr <- [|vMap $(asExp fn) $(varE (name vec))|]
    exprIntro expr

--------------------------------------------------------------------------------

modIntro :: Symbol -> [Symbol] -> ModQ Exp
modIntro m ps =
  let modExpr = foldl1 AppE (map (VarE . name) (m : ps))
   in exprIntro modExpr

modIndex :: Symbol -> Symbol -> ModQ Exp
modIndex m field =
  do
    modBinds <- getBinds
    let indexExpr = forceExpr (AppE (VarE (name field)) (VarE (name m)))
    -- We delete `field` from the module bindings because it should always
    -- refer to a record accessor, even if it happens to be previously
    -- bound in the module
    let modBinds' = delete (name field) modBinds
    withBinds modBinds' (exprIntro indexExpr)

--------------------------------------------------------------------------------

compileOptimalTypeDecls :: Env Optimal.Type -> [TypeDecl] -> Q [Dec]
compileOptimalTypeDecls tyEnv tyDecls =
  concat <$> mapM (compileOptimalTypeDecl tyEnv "m") tyDecls

compileOptimalTypeDecl :: Env Optimal.Type -> Name -> TypeDecl -> Q [Dec]
compileOptimalTypeDecl tyEnv m TypeDecl {..} =
  case ty of
    -- If we expanded to an Alias, we know it refers to a Haskell type, and so
    -- doesn't require a type synonym
    Alias _alias -> (: []) <$> tySynD (name tdName) [] (go ty)
    -- Vectors require type parameters, so their declarations do too
    Vec _inner -> (: []) <$> tySynD (name tdName) [PlainTV m ()] (go ty)
    List _inner -> (: []) <$> tySynD (name tdName) [] (go ty)
    Tuple ts -> undefined
    Arrow t1 t2 -> undefined
    Rec nm fields -> compileOptimalRecordDecl tyEnv m (name nm) fields
    App t1 t2 -> (: []) <$> tySynD (name tdName) [] (go ty)
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
    Vec t -> appT (appT (conT "Vector") (varT m)) (go t)
    List t -> appT listT (go t)
    Tuple ts -> undefined
    Arrow t1 t2 -> undefined
    Rec nm _fields -> appT (conT (name nm)) (varT m)
    App t1 t2 -> appT (go t1) (go t2)
  where
    go = compileOptimalType tyEnv m

compileThunkedOptimalType :: Env Optimal.Type -> Name -> Optimal.Type -> Q TH.Type
compileThunkedOptimalType tyEnv m ty = thunked (compileOptimalType tyEnv m ty)
  where
    thunked = appT (appT (conT "Thunked") (varT m))
