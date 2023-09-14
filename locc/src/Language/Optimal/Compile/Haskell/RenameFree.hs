{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Optimal.Compile.Haskell.RenameFree where

import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts (IsList (..))
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (ModName)
import Language.Optimal.Collection
import Language.Optimal.Compile.Haskell.CollectBindings (BindingVars, CollectBindings (..))

unimplemented :: Show a => String -> a -> b
unimplemented fn thing =
  error $
    "RenameFree: unsupported constructor in `"
      <> fn
      <> "` (failed on "
      <> take 30 (show thing)
      <> "...)"

-------------------------------------------------------------------------------

newtype FreeVars = FreeVars (Set Name)
  deriving (Eq, Show)

instance Semigroup FreeVars where
  FreeVars f1 <> FreeVars f2 = FreeVars (f1 <> f2)

instance Monoid FreeVars where
  mempty = FreeVars mempty

instance Collection FreeVars Name where
  member e (FreeVars fvs) = member e fvs
  insert e (FreeVars fvs) = FreeVars (insert e fvs)
  delete e (FreeVars fvs) = FreeVars (delete e fvs)

instance IsList FreeVars where
  type Item FreeVars = Name
  fromList names = FreeVars (Set.fromList names)
  toList (FreeVars fvs) = Set.toList fvs

-------------------------------------------------------------------------------

class RenameFree a where
  -- | `renameFree bindings f a` should modify `a` by applying `f` to every
  -- `Name` that occurs "free" in `a`, and should also include each such `Name`
  -- in the `FreeVars` result.
  renameFree :: BindingVars -> (Name -> Name) -> a -> (a, FreeVars)

instance RenameFree Exp where
  renameFree = renameFreeExp

instance RenameFree Dec where
  renameFree = renameDecFree

instance RenameFree Match where
  renameFree = renameMatchFree

instance RenameFree Body where
  renameFree = renameBodyFree

instance RenameFree Stmt where
  renameFree = renameStmtFree

instance RenameFree Guard where
  renameFree = renameGuardFree

instance RenameFree (Guard, Exp) where
  renameFree = renameGuardedExpFree

instance RenameFree Name where
  renameFree = renameNameFree

-------------------------------------------------------------------------------

renameFreeExp :: BindingVars -> (Name -> Name) -> Exp -> (Exp, FreeVars)
renameFreeExp binds f expr =
  case expr of
    VarE n
      | n `notMember` binds -> (VarE (f n), singleton n)
      | otherwise -> (expr, mempty)
    UnboundVarE n
      | n `notMember` binds -> (UnboundVarE (f n), singleton n)
      | otherwise -> (expr, mempty)
    ConE _ -> (expr, mempty)
    LitE _ -> (expr, mempty)
    AppE e1 e2 ->
      let (e1', f1s) = go e1
          (e2', f2s) = go e2
       in (AppE e1' e2', f1s <> f2s)
    AppTypeE e t ->
      let (e', frees) = go e
       in (AppTypeE e' t, frees)
    InfixE e1M e2 e3M ->
      let (e1M', f1s) = goM e1M
          (e2', f2s) = go e2
          (e3M', f3s) = goM e3M
       in (InfixE e1M' e2' e3M', f1s <> f2s <> f3s)
    UInfixE e1 e2 e3 ->
      let (e1', f1s) = go e1
          (e2', f2s) = go e2
          (e3', f3s) = go e3
       in (UInfixE e1' e2' e3', f1s <> f2s <> f3s)
    ParensE e ->
      let (e', frees) = go e
       in (ParensE e', frees)
    LamE params body ->
      let (body', frees) = go' (foldMap collectBindings params) body
       in (LamE params body', frees)
    -- -- LamCaseE matches -> undefined
    -- -- LamCasesE matches -> undefined
    TupE es ->
      let (es', frees) = unzip (map goM es)
       in (TupE es', mconcat frees)
    UnboxedTupE es ->
      let (es', frees) = unzip (map goM es)
       in (UnboxedTupE es', mconcat frees)
    UnboxedSumE e sal sar ->
      let (e', frees) = go e
       in (UnboxedSumE e' sal sar, frees)
    CondE e1 e2 e3 ->
      let (e1', f1s) = go e1
          (e2', f2s) = go e2
          (e3', f3s) = go e3
       in (CondE e1' e2' e3', f1s <> f2s <> f3s)
    MultiIfE matches ->
      let (matches', frees) = unzip (map (renameFree binds f) matches)
       in (MultiIfE matches', mconcat frees)
    ListE es ->
      let (es', frees) = unzip (map go es)
       in (ListE es', mconcat frees)
    LetE decs e ->
      let (decs', decFrees) = unzip (map go decs)
          (e', eFrees) = go' (foldMap collectBindings decs) e
       in (LetE decs' e', mconcat (eFrees : decFrees))
    CaseE e matches ->
      let (e', eFrees) = go e
          (matches', matchFrees) = unzip (map go matches)
       in (CaseE e' matches', mconcat (eFrees : matchFrees))
    DoE m stmts -> mkDo m binds stmts (mempty, mempty)
    _ -> unimplemented "renameFreeExp" expr
  where
    go :: RenameFree a => a -> (a, FreeVars)
    go = renameFree binds f

    go' :: RenameFree a => BindingVars -> a -> (a, FreeVars)
    go' binds' = renameFree (binds' <> binds) f

    goM :: RenameFree a => Maybe a -> (Maybe a, FreeVars)
    goM xM =
      case xM of
        Nothing -> (Nothing, mempty)
        Just x ->
          let (x', frees) = go x
           in (Just x', frees)

    mkDo :: Maybe ModName -> BindingVars -> [Stmt] -> ([Stmt], FreeVars) -> (Exp, FreeVars)
    mkDo m bs ss (ssAcc, freesAcc) =
      case ss of
        [] -> (DoE m (reverse ssAcc), freesAcc)
        (stmt : stmts) ->
          let (stmt', frees) = renameFree bs f stmt
              binds' = bs <> collectBindings stmt
           in mkDo m binds' stmts (stmt' : ssAcc, frees <> freesAcc)

renameDecFree :: BindingVars -> (Name -> Name) -> Dec -> (Dec, FreeVars)
renameDecFree binds f dec =
  case dec of
    ValD pat (NormalB e) decs ->
      let binds' = collectBindings pat <> foldMap collectBindings decs <> binds
          (e', eFrees) = renameFree binds' f e
          (decs', dFrees) = unzip (map (renameFree binds' f) decs)
       in (ValD pat (NormalB e') decs', mconcat (eFrees : dFrees))
    _ -> unimplemented "renameDecFree" dec

-- TODO: rename decs
renameMatchFree :: BindingVars -> (Name -> Name) -> Match -> (Match, FreeVars)
renameMatchFree binds rename mtch =
  case mtch of
    Match pat bod decs ->
      let binds' = collectBindings pat <> foldMap collectBindings decs <> binds
          (bod', frees) = renameFree binds' rename bod
       in (Match pat bod' decs, frees)

renameBodyFree :: BindingVars -> (Name -> Name) -> Body -> (Body, FreeVars)
renameBodyFree binds rename bod =
  case bod of
    NormalB e ->
      let (e', frees) = renameFree binds rename e
       in (NormalB e', frees)
    GuardedB gs ->
      let (gs', frees) = unzip (map (renameFree binds rename) gs)
       in (GuardedB gs', mconcat frees)

renameGuardedExpFree :: BindingVars -> (Name -> Name) -> (Guard, Exp) -> ((Guard, Exp), FreeVars)
renameGuardedExpFree binds rename (guard, e) =
  let (guard', guardFrees) = renameFree binds rename guard
      (e', eFrees) = renameFree (collectBindings guard') rename e
   in ((guard', e'), guardFrees <> eFrees)

renameGuardFree :: BindingVars -> (Name -> Name) -> Guard -> (Guard, FreeVars)
renameGuardFree binds f guard =
  case guard of
    NormalG e ->
      let (e', frees) = renameFree binds f e
       in (NormalG e', frees)
    PatG stmts ->
      let (stmts', frees) = unzip (map (renameFree binds f) stmts)
       in (PatG stmts', mconcat frees)

renameStmtFree :: BindingVars -> (Name -> Name) -> Stmt -> (Stmt, FreeVars)
renameStmtFree binds rename stmt =
  case stmt of
    BindS pat e ->
      let (e', frees) = renameFree (binds <> collectBindings pat) rename e
       in (BindS pat e', frees)
    LetS decs ->
      let (decs', frees) = unzip (map (renameFree binds rename) decs)
       in (LetS decs', mconcat frees)
    NoBindS e ->
      let (e', frees) = renameFree binds rename e
       in (NoBindS e', frees)
    RecS stmts ->
      let (stmts', frees) = doStmts stmts
       in (RecS stmts', frees)
    ParS stmtss ->
      let (stmtss', frees) = unzip (map doStmts stmtss)
       in (ParS stmtss', mconcat frees)
  where
    doStmts stmts =
      let (stmts', frees) = unzip (map (renameFree binds rename) stmts)
       in (stmts', mconcat frees)

renameNameFree :: BindingVars -> (Name -> Name) -> Name -> (Name, FreeVars)
renameNameFree binds rename nm =
  let nameIsBound = nm `member` binds
   in if nameIsBound then (rename nm, mempty) else (nm, singleton nm)
