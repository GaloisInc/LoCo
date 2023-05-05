{-# LANGUAGE OverloadedStrings #-}


module Language.LoCo.Samples where

import Data.Map qualified as Map
import Language.LoCo.Syntax

u8 :: Type
u8 = UnsignedTy 1

{-
type LV = { l :: U8, v :: [U8] }
-}
lvType :: Type
lvType = RecordTy "LV" (Map.fromList fields)
  where
    fields = [("l", u8), ("v", ListTy u8)]

{-
parseLV :: {- Region -> -} Parser LV
parseLV @R =
  { @RL := take 1 @R
  , l = u8 @RL
  , @RV := drop 1 @R
  , v := (many l u8) @RV
  }
-}
lvParser :: Parser
lvParser = Parser ["R"] (Map.fromList [rl, l, rv, v]) lvType
  where
    rl = ("RL", App "take" [Lit 1, "R"])
    l = ("l", RegApp "u8" "RL")
    rv = ("RV", App "drop" [Lit 1, "R"])
    v = ("v", RegApp (App "many" ["l", "u8"]) "RV")

{-
five :: Entrypoint LV U8
five = parseLV.v.4
-}
fifthElement :: Entrypoint
fifthElement = Entrypoint {..}
  where
    epTypeBase = lvType
    epTypeProjection = u8
    epParseBase = "parseLV"
    epParseProjection = [Field "v", Idx 4]

{- 
getL :: Entrypoint LV U8
getL = parseLV.l
-}
lEntry :: Entrypoint
lEntry = Entrypoint{..}
  where
    epTypeBase = lvType
    epTypeProjection = u8
    epParseBase = "parseLV"
    epParseProjection = [Field "l"]