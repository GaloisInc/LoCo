{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}

module Language.LR.Examples.ICC_Optimal where

-- base pkgs:
import           Data.Word

-- package optimal:
import           Language.Optimal.Quote (optimal)
import           Thunk.RefVal (Thunked, delayAction, delayTuple, force)
import           Thunk.Vector

-- local PEAR modules:
import           Language.LR.Examples.ICC_Spec
import           Language.LR.API
-- import           Language.PEAR.Util
-- import qualified Language.PEAR.Region.API as R -- region
import           Language.PEAR.Region.API(Region(..))


---- ICC : Optim(l-PEAR) --------------------------------------

-- Misc
--  - Aha: OverloadedRecordDot conflicts with Optimal. ?
--  - no support for nested tuple patterns?
--      , ((a,b),c)    = <| return ((1,2),3) |>

[optimal|
type ICC = { cnt       : Int
           , cnt'      : VR Int
           , rRest     : Region
           }

icc : Region -> ICC
icc rFile =
  { (cnt',rRest) = <| pInt4Bytes @! rFile |>
  , cnt = <| return (v cnt') |>
  }
|]

-- Original brainstorming:
{-
[optimal|
icc : ICC
icc rFile =
  { (cnt,rfoCnt) = <| pWord32               @@ rFile  |>,
    (tbl,     _) = <| pMany cnt.v pTblEntry @@ rfoCnt |>,
    rsTeds       = <| failP $
                       mapM (getSubRegion rFile) tbl.v |>,
    teds         = <| mapM pTED'      @  rsTeds |>,

    teds_safe    = <| if isCavityFree
                      then return teds
                      else throwE' ["teds not safe"]
                   |>,

    isCavityFree = <| verboseComplement rFile crFile |>,
    crFile       = <| makeCanonicalRegions
                        (cnt.r : tbl.r : rsTeds)
                   |>,
  }
|]

-}
