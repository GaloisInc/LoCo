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
import qualified Language.PEAR.Region.API as R -- region
import           Language.PEAR.Region.API(Region(..))


---- ICC : Optim(l-PEAR) --------------------------------------

-- Misc
--  - Aha: OverloadedRecordDot conflicts with Optimal. ?
--  - no support for nested tuple patterns: (?)
--      , ((a,b),c)    = <| return ((1,2),3) |>
--  - no support for '_' bindings.

[optimal|
type ICC = { cnt       : VR Int
           , rRest     : Region
           , teds      : [TED]
           , teds_safe : [TED]
           }

icc : Region -> ICC
icc rFile =
  { (cnt,rRest) = <| pInt4Bytes                  @! rFile          |>
  , tbl         = <| pManySRPs (v cnt) pTblEntry @$ rRest          |>
  , rsTeds      = <| except $ mapM (getSubRegion rFile) (v tbl)    |>
  , teds        = <| mapM applyPTED rsTeds                         |>
  , crsFile     = <| makeCanonicalRegions (r cnt : r tbl : rsTeds) |>
  , isCavityFree= <| hasNoCavities $ R.complementCRs rFile crsFile |>
  , teds_safe   = <| if isCavityFree
                       then return teds
                       else throwE ["teds not safe"] |>
  }
|]
