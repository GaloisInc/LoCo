{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE TypeFamilies #-}

module Language.Optimal.Compile.FreeVars where

import Data.Maybe (catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Exts
import Language.Haskell.TH

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
  deriving (Eq, Show)

newtype BindingVars = BindingVars (Set Name)
  deriving (Eq, Show)

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

instance IsList FreeVars where
  type Item FreeVars = Name
  fromList names = FreeVars (Set.fromList names)
  toList (FreeVars fvs) = Set.toList fvs

instance IsList BindingVars where
  type Item BindingVars = Name
  fromList names = BindingVars (Set.fromList names)
  toList (BindingVars bvs) = Set.toList bvs

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
    UnboundVarE n
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
    MultiIfE matches -> foldMap (\(guard, e) -> goGuard guard <> goExp' (guardBindings guard) e) matches
    ListE es -> foldMap goExp es
    LetE decs e -> foldMap goDec decs <> goExp' (foldMap decBindings decs) e
    CaseE e matches -> goExp e <> foldMap goMatch matches
    DoE _ [] -> mempty
    DoE m (stmt : stmts) -> goStmt stmt <> goExp' (stmtBindings stmt) (DoE m stmts)
    _ -> error $ "TODO: finish constructors in `expFreeVars` (failed on " <> take 30 (show expr) <> "...)"
  where
    goExp = expFreeVars bindings
    goExp' bindings' = expFreeVars (bindings' <> bindings)
    goDec = decFreeVars bindings
    goMatch = matchFreeVars bindings
    goGuard = guardFreeVars bindings
    goStmt = stmtFreeVars bindings

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
    GuardedB gs -> foldMap (\(guard, e) -> goGuard guard <> goExp (guardBindings guard) e) gs
  where
    goGuard = guardFreeVars bindings
    goExp bindings' = expFreeVars (bindings' <> bindings)

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
