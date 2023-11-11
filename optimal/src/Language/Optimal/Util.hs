{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Optimal.Util where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH (Name, mkName)

import Control.Monad
import Data.Either
import Data.Function ((&))
import Data.Graph.Inductive.Graph hiding ((&))
import Data.Graph.Inductive.PatriciaTree (Gr) -- best implem
import Data.Graph.Inductive.Query.DFS
import Data.List

class Named a where
  name :: a -> Name

instance Named String where
  name = mkName

instance Named Text where
  name = mkName . Text.unpack

instance Named Name where
  name = id

instance IsString Name where
  fromString = mkName

---- topological sort --------------------------------------------------------

topoSortPossibly :: forall a m. (MonadFail m, Show a, Eq a) => (a -> a -> Bool) -> [(a, [a])] -> m [a]
topoSortPossibly refersTo ns =
  do
    let nms = map fst ns
    when (length (nub nms) /= length nms) $
      fail $
        unwords ["duplicate entries"] -- FIXME: improve msg
    mapM_ failIfRefersToSelf ns

    let canMakeDest d =
          case findIndex (`refersTo` d) nms of
            Just i -> return i
            Nothing -> fail $ unwords ["reference to", show d, "is non-existent"]
        -- NB: zero-based

        canMakeNode (i, (nm, ds)) =
          do
            ds' <- mapM canMakeDest ds
            return
              ( (i, nm),
                map (\d -> (i, d, ())) ds'
              )

    ns' <- mapM canMakeNode $ zip [0 ..] ns

    let gr :: Gr a () -- indicate the Graph implementation
        gr = mkGraph (map fst ns') (concatMap snd ns')

    mapM_ (failIfCycle nms) (scc gr)
    return (topsort' gr)
  where
    failIfCycle nms =
      \case
        [] -> fail "isCycle"
        [_] -> return ()
        ns'' -> fail $ unwords ["entries form a cycle", show nms']
          where
            nms' = map (nms !!) ns''

    failIfRefersToSelf (a, as) =
      when (a `elem` as) $
        fail $
          unwords ["entry", show a, "refers to itself"]

---- testing -----------------------------------------------------------------

topoSortPossibly' ::
  forall a. (Show a, Eq a) => [(a, [a])] -> Either String [a]
topoSortPossibly' = topoSortPossibly (==)

prop_test1 :: Bool
prop_test1 = and tests1

tests1 :: [Bool]
tests1 =
  [ topoSortPossibly' [('a', "ab")] & isLeft, -- "entry 'a' refers to itself"
    topoSortPossibly' [('a', "b")] & isLeft, -- "reference ... is non-existent"
    topoSortPossibly' [('a', "b"), ('b', "a")] & isLeft, -- "entries form a cycle"
    topoSortPossibly' [('a', "b"), ('b', "")] == Right "ab",
    topoSortPossibly' [('c', ""), ('a', "b"), ('b', "")] == Right "abc",
    topoSortPossibly' [('c', "b"), ('a', "b"), ('b', "")] == Right "acb",
    topoSortPossibly' [('c', ""), ('a', "b"), ('b', "c")] == Right "abc"
  ]

---- orphan instance: --------------------------------------------------------

instance MonadFail (Either String) where
  fail = Left
