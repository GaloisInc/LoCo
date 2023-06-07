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
  case thunkBinds of
    [] -> pure expr
    _ -> doE (thunkBinds <> [noBindS (pure expr)])
  where
    thunkVars = free expr `Set.intersection` Set.map mkName' modVars
    thunkBinds =
      [ bindS (varP =<< newName (show thunkVar)) [|force $(varE thunkVar)|]
        | thunkVar <- Set.toList thunkVars
      ]

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

free :: Exp -> Set Name
free = freeWith mempty

freeWith :: Set Name -> Exp -> Set Name
freeWith binders expr =
  case expr of
    VarE n
      | n `Set.notMember` binders -> Set.singleton n
      | otherwise -> mempty
    ConE n -> mempty
    LitE l -> mempty
    AppE e1 e2 -> go e1 <> go e2
    AppTypeE e t -> go e
    InfixE e1M e2 e3M -> maybe mempty go e1M <> go e2 <> maybe mempty go e3M
    UInfixE e1 e2 e3 -> go e1 <> go e2 <> go e3
    ParensE e -> go e
    LamE params body -> freeWith (foldMap matchedNames params) body
    LamCaseE matches -> undefined
    -- LamCasesE matches -> undefined
    TupE es -> foldMap go (catMaybes es)
    UnboxedTupE es -> foldMap go (catMaybes es)
    UnboxedSumE e _ _ -> go e
    CondE e1 e2 e3 -> go e1 <> go e2 <> go e3
    _ -> undefined
  where
    go = freeWith binders

matchedNames :: Pat -> Set Name
matchedNames pat =
  case pat of
    LitP l -> mempty
    VarP n -> Set.singleton n
    TupP ps -> foldMap matchedNames ps
    UnboxedTupP ps -> foldMap matchedNames ps
    UnboxedSumP p _ _ -> matchedNames p
    ConP _ _ ps -> foldMap matchedNames ps
    -- this `n` represents, e.g., the `:` in a pattern like (x:xs)
    InfixP p1 n p2 -> matchedNames p1 <> matchedNames p2
    UInfixP p1 n p2 -> matchedNames p1 <> matchedNames p2
    ParensP p -> matchedNames p
    TildeP p -> matchedNames p
    BangP p -> matchedNames p
    AsP n p -> Set.insert n (matchedNames p)
    WildP -> mempty
    RecP recName fieldPats -> foldMap (\(n, p) -> matchedNames p) fieldPats
    ListP ps -> foldMap matchedNames ps
    SigP p _ -> matchedNames p
    ViewP e p -> matchedNames p

-- _ -> undefined