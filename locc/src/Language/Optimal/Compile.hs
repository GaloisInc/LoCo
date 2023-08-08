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
    orderedModuleBinds <- sortModuleBindings modEnv'
    binds <- compileModuleBinds modBinds orderedModuleBinds
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

sortModuleBindings :: MonadFail m => Map Name Exp -> m [(Name, Exp)]
sortModuleBindings modEnv =
  do
    let dependencies =
          [ (var, deps)
            | (var, expr) <- Map.toList modEnv,
              let deps = Set.toList (thunks modNames expr)
          ]
    orderedNames <- reverse <$> topoSortPossibly dependencies
    pure [(name, modEnv Map.! name) | name <- orderedNames]
  where
    modNames = Map.keysSet modEnv

compileModuleBinds :: Set Name -> [(Name, Exp)] -> Q [Stmt]
compileModuleBinds modBinds orderedModBinds =
  sequence [bind name expr | (name, expr) <- orderedModBinds]
  where
    bind name expr = bindS (varP name) [|delayAction $(compileExpr modBinds expr)|]

-- - Collect all of the free variables in this expression that are also variables
--   in the encompassing module
-- - Create a do expression that:
--   - Forces each of them
--   - Ends with the user-provided expression
compileExpr :: Set Name -> Exp -> Q Exp
compileExpr modBinds expr =
  do
    renaming <- thunkRenaming modBinds expr
    let thunkBinds = thunkBindings renaming
        update n = fromMaybe n (renaming Map.!? n)
        expr' = rename update expr
    case thunkBinds of
      [] -> pure expr'
      _ -> pure (DoE Nothing (thunkBinds <> [NoBindS expr']))

-- | Create statements that bind each fresh name in the map to the `force`d
-- original
thunkBindings :: Map Name Name -> [Stmt]
thunkBindings renamingMap =
  [stmt original fresh | (original, fresh) <- Map.toList renamingMap]
  where
    stmt original fresh =
      BindS (VarP fresh) (AppE (VarE (mkName "force")) (VarE original))

thunks :: Set Name -> Exp -> Set Name
thunks modBinds expr = freeVars expr `Set.intersection` modBinds

-- | Create a renaming strategy for the variables in an expression that refer to
-- a binding in the surrounding module.
thunkRenaming :: Set Name -> Exp -> Q (Map Name Name)
thunkRenaming modBinds expr =
  let thunkedVariables = thunks modBinds expr
   in sequence (Map.fromList [(tv, newName (show tv)) | tv <- Set.toList thunkedVariables])

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
        _ -> compileOptimalTySynDecl (mkName' tdName) tdType
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

compileOptimalTySynDecl :: Name -> Optimal.Type -> Q Dec
compileOptimalTySynDecl name ty = tySynD name [] (compileOptimalType ty)

compileOptimalType :: Optimal.Type -> Q TH.Type
compileOptimalType pty =
  case pty of
    Alias s -> conT (mkName' s)
    List ty ->
      let ty' = compileOptimalType ty
       in [t|[$ty']|]
    Tuple tys -> foldl1 appT (tupleT (length tys) : map compileOptimalType tys)
    Arrow t1 t2 ->
      let t1' = compileOptimalType t1
          t2' = compileOptimalType t2
       in [t|$t1' -> $t2'|]
    Rec _ -> fail "can't compile record type in non-declaration context"

mkName' :: Text -> Name
mkName' = mkName . Text.unpack
