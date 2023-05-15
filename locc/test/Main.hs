{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-orphans #-}

{-# HLINT ignore "Use camelCase" #-}

module Main where

import Control.Monad (replicateM)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Language.LoCoEssential.Essence (Env, FreeVars, LoCoModule (..), RHS (RHSExpr), Symbol, fvs)
import Language.LoCoEssential.Interp.Lazy qualified as Lazy
import Language.LoCoEssential.Interp.Trad qualified as Trad
import Language.LoCoEssential.SimpleExpr.Expr qualified as Simple
import Test.Tasty (TestTree, defaultMain)
import Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testProperty "" lazyMatchesTraditional_prop

lazyMatchesTraditional_prop :: LoCoModule Simple.Expr -> Property
lazyMatchesTraditional_prop = ioProperty . lazyMatchesTraditional

lazyMatchesTraditional :: LoCoModule Simple.Expr -> IO Bool
lazyMatchesTraditional lMod =
  do
    tradMod <- Trad.interpret lMod
    lazyModThunks <- Lazy.interpret lMod
    lazyMod <- sequence lazyModThunks
    pure (Map.toList tradMod == Map.toList lazyMod)

instance Arbitrary Simple.Expr where
  -- EAdd: 40%
  -- ELit: 30%
  -- EVar: 30%
  arbitrary =
    do
      i <- chooseInt (1, 10)
      if
          | i <= 4 -> Simple.EAdd <$> arbitrary <*> arbitrary
          | i <= 7 -> Simple.ELit <$> arbitrary
          | otherwise -> Simple.EVar <$> arbitrary

instance (Arbitrary e, FreeVars e) => Arbitrary (LoCoModule e) where
  arbitrary =
    do
      let lModName = "<arbitrary>"
      (e1, e2, e3) <- arbitrary
      (s1, s2, s3) <- arbitrary
      let env = Map.fromList [(s1, e1), (s2, e2), (s3, e3)]
      env' <- bindFreeVars (concatMap (Set.toList . fvs) [e1, e2, e3])
      let lModBinds = RHSExpr <$> (env <> env')
      pure LoCoModule {..}

-- | Recursively generate an environment in which each of the free variables in
-- the list is bound to an (arbitrary) expression. Ensure termination by
-- recursively generating expressions with substantially fewer free variables
-- than the length of the original list.
bindFreeVars :: (Arbitrary e, FreeVars e) => [Symbol] -> Gen (Env e)
bindFreeVars vars =
  case vars of
    [] -> pure mempty
    (v : vs) ->
      do
        (expr, itsFreeVars) <- arbitrary `suchThatMap` fvsUpperBound bound
        env <- bindFreeVars (Set.toList itsFreeVars)
        env' <- bindFreeVars vs
        pure (Map.insert v expr (env <> env'))
  where
    bound = length vars `div` 2

fvsUpperBound :: FreeVars e => Int -> e -> Maybe (e, Set Symbol)
fvsUpperBound i e =
  let frees = fvs e
   in if length frees <= i then Just (e, frees) else Nothing

instance {-# OVERLAPPING #-} Arbitrary Symbol where
  arbitrary =
    do
      len <- choose (8, 16)
      suffix <- replicateM len (elements chars)
      pure ('v' : suffix)
    where
      chars = ['A' .. 'Z'] ++ ['a' .. 'z']
