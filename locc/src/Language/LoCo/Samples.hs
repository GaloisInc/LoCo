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
parseLV :: Parser LV
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
    l = ("l", App "u8" ["RL"])
    rv = ("RV", App "drop" [Lit 1, "R"])
    v = ("v", App (App "many" ["l", "u8"]) ["RV"])

{-
fifthElement :: Entrypoint LV U8
fifthElement = parseLV.v.4
-}
fifthElement :: Entrypoint
fifthElement = Entrypoint {..}
  where
    epTypeBase = lvType
    epTypeProjection = u8
    epParseBase = "parseLV"
    epParseProjection = ["v", "4"]
