{-# LANGUAGE QuasiQuotes #-}

module Language.OptimalPEAR.Examples.ICC_Optimal where

-- base pkgs:
import           Control.Monad.IO.Class
import           Data.Word

-- package optimal:
import           Language.Optimal.Quote (optimal,optimalVerbose)
import           Thunk.RefVal (Thunked, delayAction, delayTuple, force)
import           Thunk.Vector

-- local PEAR modules:
import           Language.OptimalPEAR.Examples.ICC_Spec
import           Language.PEAR.API
import qualified Language.PEAR.Region.API as R
import           Language.PEAR.Region.API(Region(..))

import           Language.OptimalPEAR.RunOptimal

-- to get input for runing the demos, bring this into scope:
-- import           Language.OptimalPEAR.Examples.ICC_Inputs

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

run_ICC_primList = run' (flip runPT) icc iccPrims
runp prog = run_ICC_primList prog

clientProgNoVa = ["cnt","rRest","tbl","teds","teds_safe"]

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
-- FIXME: TODO[C3]
--   - get rid of VR

[optimalVerbose|
type ICC_V = { cnt'       : VR Int
             , rRest'     : Region
             , rsTbl      : Vec<Region>
             , tbl'       : Vec<TBLEntry>
             , teds'      : Vec<TED>
             , teds'_0    : TED
             , teds'_1    : TED
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
  , teds'_0       = index teds' i_0
  , teds'_1       = index teds' i_1
  }
|]

-- Optimal: Wants/Issues
--  - nice to have ints, to apply to index
--  - allows duplicate names in both the module and in the type!
--  - handle index errors "ourselves", rather not this:
--     runp_v ["teds''_0"] d1
--     ...
--     *** Exception: index out of bounds (0,0)

i_0 = 0 :: Int
i_1 = 1 :: Int

icc_v :: MonadIO m => Region -> PT m (ICC_V (PT m))

p @$$ r = v <$> appSRP p r  -- ^ parse whole region, exactly, toss region

-- FIXME[C1]: turn into a more generic slice & dice combinator (change nm at least)
getTblRegion cnt r w i = do
                         (vs,_) <- except $ R.splitNWidthP cnt w r
                         return (vs !! i )

---- ICC_V Demo ----------------------------------------------------

run_ICC_V_primList = run' (flip runPT) icc_v iccPrims_v

runp_v clientProg = run_ICC_V_primList clientProg

clientProgA = ["cnt'","tbl'","tbl'Elems"]
clientProgB = ["cnt'","rRest'","rsTbl","rsTblElems"]
clientProgC = ["cnt'","rRest'","rsTbl","tbl'Elems"]
clientProgD = ["cnt'","tbl'","teds'","teds'_0"]
clientProgE = ["cnt'","teds''_0","teds''_0"]
clientProgE' = clientProgE ++ ["teds''_1","teds''_1"]

iccPrims_v :: MonadIO m => [(String, ICC_V m -> m String)]
iccPrims_v =
  [ ("cnt'"      , forceAndShow        . cnt')
  , ("rRest'"    , forceAndShow        . rRest')
  , ("rsTbl"     , forceAndShow        . rsTbl)
  , ("rsTblElems", forceAndShowVec     . rsTbl)
  , ("tbl'"      , forceAndShow        . tbl')
  , ("tbl'Elems" , forceAndShowVec     . tbl')
  , ("teds'"     , forceAndShow        . teds')  -- doesn't show much, a Vec
  , ("teds'_0"   , forceAndShow        . teds'_0)
  , ("teds'_1"   , forceAndShow        . teds'_1)
  , ("teds''_0"  , flip indexAndShow 0 . teds')
  , ("teds''_1"  , flip indexAndShow 1 . teds')
  ]


-- NOTE that lazy vectors are working:
--   take 25 d3 -- remove the teds[1] from the file.
--
--   ghci> runp_v ["teds'_0"] (take 25 d3)
--   ...
--   "teds'_0" evaluated to TED "abcde"
--   program exited cleanly
--   ghci> runp_v ["teds'_1"] (take 25 d3)
--   ...
--   program exited with:
--   no subregion (25,3) of region R 0 25
