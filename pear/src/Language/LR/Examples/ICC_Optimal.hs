{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.LR.Examples.ICC_Optimal where

-- base pkgs:
import           Control.Monad.IO.Class
import           Data.Word

-- transformer pkg:

-- package optimal:
import           Language.Optimal.Quote (optimal)
import           Thunk.RefVal (Thunked, delayAction, delayTuple, force)
import           Thunk.Vector

-- local PEAR modules:
import           Language.LR.Examples.ICC_Spec
import           Language.LR.API
import qualified Language.PEAR.Region.API as R
import           Language.PEAR.Region.API(Region(..))

import           Language.OptimalPEAR.RunOptimal

-- merely bringing into scope for demo:
import           Language.OptimalPEAR.Examples.ICC_Inputs

---- ICC : Optim(l-PEAR), not using vectors ------------------------

-- Misc
--  - Aha: OverloadedRecordDot conflicts with Optimal. ?
--  - no support for nested tuple patterns: (?)
--      , ((a,b),c)    = <| return ((1,2),3) |>
--  - no support for '_' bindings.
--  - cannot inline type TBL

type TBL = [(Word32,Word32)]

[optimal|
type ICC = { cnt       : VR Int
           , rRest     : Region
           , tbl       : VR TBL
           , teds      : [TED]
           , teds_safe : [TED]
           }

icc : Region -> ICC
icc rFile =
  { (cnt,rRest) = <| pInt4Bytes                     @!  rFile      |>
  , tbl         = <| pManySRPs (v cnt) pTblEntry    @!- rRest      |>
  , rsTeds      = <| except $ mapM (getSubRegion rFile) (v tbl)    |>
  , teds        = <| mapM applyPTED rsTeds                         |>
  , crsFile     = <| makeCanonicalRegions (r cnt : r tbl : rsTeds) |>
  , isCavityFree= <| hasNoCavities $ R.complementCRs rFile crsFile |>
  , teds_safe   = <| if isCavityFree
                       then return teds
                       else throwE ["teds not safe"] |>
  }
|]

icc :: MonadIO m => Region -> PT m (ICC (PT m))


---- ICC Demo ------------------------------------------------------

run_ICC_d1 = run_ICC d1

run_ICC = run' (flip runPT) icc iccPrims
               ["cnt","rRest","tbl","teds","teds_safe"]

iccPrims :: MonadIO m => [(String, ICC m -> m String)]
iccPrims =
  [ ("cnt"       , forceAndShow . cnt)
  , ("rRest"     , forceAndShow . rRest)
  , ("tbl"       , forceAndShow . tbl)
  , ("teds"      , forceAndShow . teds)
  , ("teds_safe" , forceAndShow . teds_safe)
  ]


---- ICC_V : Optim(l-PEAR), Using vectors ------------------------

type TBLEntry = (Word32,Word32)
-- FIXME: TODO
--   - finish translating to vectors
--   - get rid of VR

[optimal|
type ICC_V = { cnt'       : VR Int
             , rRest'     : Region
             , rsTbl      : Vec<Region>
             , tbl'       : Vec<TBLEntry>
             , teds'      : Vec<TED>
             }

icc_v : Region -> ICC_V
icc_v rFile =
  { (cnt',rRest') = <| pInt4Bytes                      @!  rFile       |>
  , cntv          = <| pure (v cnt')                                   |>
  , rsTbl         = generate cntv <| \i-> getTblRegion cntv rRest'
                                          (srpW' pTblEntry) i           |>
  , tbl'          = map rsTbl  <| \r-> pTblEntry @$$ r                 |>
  , rsTeds        = map tbl'   <| \r-> except $ getSubRegion rFile r   |>
  , teds'         = map rsTeds <| applyPTED                            |>
  }
|]

icc_v :: MonadIO m => Region -> PT m (ICC_V (PT m))

p @$$ r = v <$> appSRP p r  -- ^ parse whole region, exactly, toss region

-- FIXME[C1]: turn into a more generic slice & dice combinator (change nm at least)
getTblRegion cnt r w i = do
                         (vs,_) <- except $ R.splitNWidthP cnt w r
                         return (vs !! i )

---- ICC_V Demo ----------------------------------------------------

run_ICC_V_primList = run' (flip runPT) icc_v iccPrims_v
run_ICC_V_clienta = run_ICC_V_primList ["cnt'","tbl'","tbl'Elems"]
run_ICC_V_clientb = run_ICC_V_primList ["cnt'","rRest'","rsTbl","rsTblElems"]
run_ICC_V_clientc = run_ICC_V_primList ["cnt'","rRest'","rsTbl","tbl'Elems"]
run_ICC_V_clientd = run_ICC_V_primList ["cnt'","tbl'","teds'","teds'_0"]
run_ICC_V_cliente = run_ICC_V_primList ["cnt'","teds'_0"]

iccPrims_v :: MonadIO m => [(String, ICC_V m -> m String)]
iccPrims_v =
  [ ("cnt'"      , forceAndShow    . cnt')
  , ("rRest'"    , forceAndShow    . rRest')
  , ("rsTbl"     , forceAndShow    . rsTbl)
  , ("rsTblElems", forceAndShowVec . rsTbl)
  , ("tbl'"      , forceAndShow    . tbl')
  , ("tbl'Elems" , forceAndShowVec . tbl')
  , ("teds'"     , forceAndShow . teds') -- ??
  , ("teds'_0"   , flip indexAndShow 0 . teds')
  ]
