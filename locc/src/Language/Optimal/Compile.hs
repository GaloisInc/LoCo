{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Optimal.Compile where

import Control.Monad.IO.Class
import Data.Map qualified as Map
import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text
import GHC.Exts
import Language.Haskell.TH
import Language.Haskell.TH qualified as TH
import Language.LoCo.Toposort (topoSortPossibly)
import Language.Optimal.Compile.FreeVars (freeVars)
import Language.Optimal.Compile.Rename (renameExp)
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
    -- pure [sig, decl]
    pure [decl]
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
    List ty -> [t|[$(compileOptimalType ty)]|]
    Tuple tys -> foldl1 appT (tupleT (length tys) : map compileOptimalType tys)
    Alias s -> conT (mkName' s)

mkName' :: Text -> Name
mkName' = mkName . Text.unpack
