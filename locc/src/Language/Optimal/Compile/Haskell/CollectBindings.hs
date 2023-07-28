module Language.Optimal.Compile.Haskell.CollectBindings (CollectBindings (..)) where

import Language.Haskell.TH
import Language.Optimal.Compile.Collections

unimplemented :: Show a => String -> a -> b
unimplemented fn thing =
  error $
    "TODO: CollectBindings: finish constructors in `"
      <> fn
      <> "` (failed on "
      <> take 30 (show thing)
      <> "...)"

-------------------------------------------------------------------------------

class CollectBindings a where
  collectBindings :: a -> BindingVars

instance CollectBindings Dec where
  collectBindings = decBindings

instance CollectBindings Pat where
  collectBindings = patBindings

instance CollectBindings Guard where
  collectBindings = guardBindings

instance CollectBindings Stmt where
  collectBindings = stmtBindings

-------------------------------------------------------------------------------

decBindings :: Dec -> BindingVars
decBindings dec =
  case dec of
    ValD pat body _ -> patBindings pat
    _ -> unimplemented "decBindings" dec

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

guardBindings :: Guard -> BindingVars
guardBindings guard =
  case guard of
    NormalG _ -> mempty
    PatG stmts -> foldMap stmtBindings stmts

stmtBindings :: Stmt -> BindingVars
stmtBindings stmt =
  case stmt of
    BindS pat _ -> patBindings pat
    LetS decs -> foldMap decBindings decs
    NoBindS _ -> mempty
    ParS stmtss -> foldMap (foldMap stmtBindings) stmtss
    RecS stmts -> foldMap stmtBindings stmts
