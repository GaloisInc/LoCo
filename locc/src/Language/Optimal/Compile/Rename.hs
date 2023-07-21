module Language.Optimal.Compile.Rename where

import Language.Haskell.TH

-- XXX: does not guard against changing bound names!

renameExp :: (Name -> Q Name) -> Exp -> Q Exp
renameExp f expr =
  case expr of
    VarE n -> VarE <$> f n
    ConE n -> ConE <$> f n
    LitE l -> pure expr
    AppE e1 e2 -> appE (go e1) (go e2)
    ParensE e -> parensE (go e)
    CondE e1 e2 e3 -> condE (go e1) (go e2) (go e3)
    LetE decs e -> letE (map goDec decs) (go e)
    UInfixE e1 e2 e3 -> uInfixE (go e1) (go e2) (go e3)
    ListE es -> listE (map go es)
    CaseE e matches -> caseE (go e) (map goMatch matches)
    _ -> error $ "TODO: finish constructors in `renameExp` (failed on " <> take 30 (show expr) <> "...)"
  where
    -- TODO: etc

    go = renameExp f
    goDec = renameDec f
    goMatch = renameMatch f

renameMatch :: (Name -> Q Name) -> Match -> Q Match
renameMatch f m =
  case m of
    Match pat (NormalB expr) decs ->
      match (renamePat f pat) (normalB (renameExp f expr)) (map (renameDec f) decs)
    _ -> error $ "TODO: finish constructors in `renameMatch` (failed on " <> take 30 (show m) <> "...)"

renameDec :: (Name -> Q Name) -> Dec -> Q Dec
renameDec f dec =
  case dec of
    ValD pat (NormalB e) wheres -> valD (renamePat f pat) (normalB (renameExp f e)) (map pure wheres)
    _ -> error $ "TODO: finish constructors in `renameDec` (failed on " <> take 30 (show dec) <> "...)"

renamePat :: (Name -> Q Name) -> Pat -> Q Pat
renamePat f pat =
  case pat of
    LitP l -> litP l
    VarP n -> VarP <$> f n
    ConP ctor tyArgs patArgs ->
      ConP
        <$> f ctor
        <*> pure tyArgs -- shouldn't need to rename types...
        <*> mapM (renamePat f) patArgs
    _ -> error $ "TODO: finish constructors in `renamePat` (failed on " <> take 30 (show pat) <> "...)"

-- TupP ps -> foldMap patBindings ps
-- UnboxedTupP ps -> foldMap patBindings ps
-- UnboxedSumP p _ _ -> patBindings p
-- ConP _ _ ps -> foldMap patBindings ps
-- -- this `n` represents, e.g., the `:` in a pattern like (x:xs)
-- InfixP p1 n p2 -> patBindings p1 <> singleton n <> patBindings p2
-- UInfixP p1 n p2 -> patBindings p1 <> singleton n <> patBindings p2
-- ParensP p -> patBindings p
-- TildeP p -> patBindings p
-- BangP p -> patBindings p
-- AsP n p -> insert n (patBindings p)
-- WildP -> mempty
-- RecP recName fieldPats -> foldMap (\(n, p) -> patBindings p) fieldPats
-- ListP ps -> foldMap patBindings ps
-- SigP p _ -> patBindings p
-- ViewP e p -> patBindings p
