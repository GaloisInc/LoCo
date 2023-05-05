{-# LANGUAGE DeriveGeneric #-}
{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Language.LoCoV2.Syntax.Simple where

import Control.Monad.Except
import Control.Monad.Reader (Reader)
import Data.Hashable (Hashable (hashWithSalt))
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import GHC.Generics (Generic)
import Language.LoCo.Region (Region)

type Ident = String

newtype Format = Format (Map Ident Thunk)

newtype Thunk = Thunk Expr
  deriving (Eq, Generic)

onExpr :: Thunk -> (Expr -> a) -> a
onExpr (Thunk e) f = f e

instance Hashable Thunk

data Expr
  = ELit Literal
  | EVar Ident
  | EPrim Primitive
  | EApp Expr [Expr]
  deriving (Eq, Generic)

instance Hashable Expr

data Value
  = VLit Literal
  | VPrim Primitive
  | VVecStrict [Value]
  | VVecLazy [Thunk]

data Literal
  = LInt Int
  | LRegion Region
  deriving (Eq, Generic)

instance Hashable Literal

data Primitive
  = Parse ParseTy
  | Take
  | Drop
  | Many
  | Index
  deriving (Eq, Generic)

instance Hashable Primitive

data ParseTy = Unsigned Int
  deriving (Eq, Generic)

instance Hashable ParseTy

u8 :: ParseTy
u8 = Unsigned 1

{-

parseLV :: Region -> Parser LV
parseLV R =
  { RL := take 1 R
  , RV := drop 1 R
  , l = u8 RL
  , v = many l u8 RV
  }

selecting l and v.3

-}

lvFormat :: Format
lvFormat =
  Format
    ( Map.fromList
        [(i, Thunk e) | (i, e) <- [(rl, rlExpr), (rv, rvExpr), (l, lExpr), (v, vExpr)]]
    )
  where
    r = "R"

    rl = "RL"
    rv = "RV"
    l = "l"
    v = "v"

    rlExpr = EApp (EPrim Take) [int 1, EVar r]
    rvExpr = EApp (EPrim Drop) [int 1, EVar r]
    lExpr = EApp (EPrim (Parse u8)) [EVar rl]
    vExpr = EApp (EPrim Many) [EVar l, EPrim (Parse u8), EVar rv]

    int = ELit . LInt

-- | Filter bindings that needn't be evaluated to produce the specified
-- bindings.
--
-- We might want to introduce a new type for the result of this. We
-- might also want to generalize the accesses we allow here via `Set Ident`, to
-- e.g. indexing a list: data Accessor = Ident Ident | Index Int
select :: Format -> Set Ident -> Format
select = undefined

-- Under what circumstances would the result of `select` be amenable to
-- reduction/elimination of thunks?
--
-- For a field's thunk to be eliminable:
-- - The field must *not* be one that the user has selected
-- - The field must be referenced by precisely one of the fields the user has
--   selected
--   - It cannot be referenced zero times, or it would have been eliminated by
--     `select`
--   - It cannot be referenced more than once, or else we want to maintain
--     "standard" thunking behavior

-- A possible result of `select parseLV ["l"]`:
lFormat :: Format
lFormat =
  let (Format lvBindings) = lvFormat
   in Format (Map.restrictKeys lvBindings (Set.fromList ["RL", "L"]))

-- Now, we can eliminate the thunk on "RL". Should this be done by inlining the
-- expression?
-- - Depends on how we deal with caching thunks. Probably time to consider a
--   method of evaluation...

-- A possible result of `select parseLV ["v.3"]:
-- - How do we expose v.3 to the user?
v3Format :: Format
v3Format =
  let (Format lvBindings) = lvFormat
   in undefined

-- What about translating from Map Ident Thunk to, like, Map Ident (ParserM Value)?