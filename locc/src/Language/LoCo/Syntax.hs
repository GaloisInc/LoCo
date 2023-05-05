{-# HLINT ignore "Use newtype instead of data" #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Language.LoCo.Syntax where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.String (IsString (..))
import Language.Haskell.TH qualified as TH

type Ident = String

{-
type LV = { l :: U8, v :: [U8] }

parseLV :: Parser LV
parseLV @R =
  { @RL := take 1 @R      -- Failure mode: @R is not large enough
  , l = u8 @RL            -- Failure mode: none
  , @RV := drop 1 @R      -- Failure mode: @R is not large enough?
                          -- But if we got to here, we parsed `l`, which means we
                          -- took 1 from the region, which means we can drop 1 from
                          -- it as well...
  , v := (many l u8) @RV  -- Failure mode: @RV is not large enough
  }

fifthElement :: Entrypoint LV U8
fifthElement = parseLV.v.4  -- Failure mode: those of `parseLV`, _as well as_ `l`
                            -- being less than 5

generating this entrypoint must generate preconditions
-}

data Type
  = UnsignedTy {unsignedWidth :: Int}
  | RecordTy {recordName :: Ident, recordFieldTys :: Map Ident Type}
              -- NOTE[mt]: recordName?
  | ListTy {listInner :: Type} -- length?
  | ParserTy { ptyResult :: Type}
      -- NOTE[mt]: dispense with?
      
-- NOTE[mt]:
--  - separate types for
--    - something bound in the pBinds?
--    - the RecordTy?
--  - hmmm
--    - f :: *->* parameter, so we can apply Thunk?
--

data Expr
  = Lit Integer
  | Var Ident
  | App Expr [Expr]
  | RegApp Expr Ident
  deriving (Show)


vars :: Expr -> [Ident]
vars expr =
  case expr of
    Lit _ -> mempty
    Var v -> [v]
    App e es -> vars e <> foldMap vars es
    RegApp e v -> vars e <> [v]

prims :: Map Ident (TH.Q TH.Exp)
prims =
  Map.fromList
    [ ("take", [|rTake|]),
      ("drop", [|rDrop|]),
      ("u8", [|parseU8|]),
      ("many", [|manyT|])
    ]

instance IsString Expr where
  fromString = Var

data Parser = Parser
  { pRegionParams :: [Ident],
    pBinds :: Map Ident Expr,
    pResult :: Type
  }


-- lazy vector: Thunk [a], or [Thunk a]

data Entrypoint = Entrypoint
  { epTypeBase :: Type,
    epTypeProjection :: Type,
    epParseBase :: Ident,
    epParseProjection :: [Accessor]
  }
  -- NOTE[mt]:
  --  - better name maybe?  '{Demand/Get}Entry' 
  --  - This would eventually be inside Expr?
 
data Accessor = Field String | Idx Int

-------------------------------------------------------------------------------

{-
For the future:

-- Parsers with explicit, type-level region width constraints:
parseLV :: fin N, N >= 1 => Parser (Region N) LV
parseLV @R =
  { let @RL = take 1 @R
  , let @RV = drop 1 @R
  , l := u8 @RL
  , v := u8 @RV
  }

type PLV = { p :: U8, lv :: LV }

-- Parsers with seek:
parsePLV :: SeekParser (Region 1) PLV
parsePLV @R =
  { p := u8 @R
  , &RLV := seek p  -- a syntax for "partial regions"?
  , lv := parseLV &RLV
  }
 -}
