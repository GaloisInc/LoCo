{-# LANGUAGE OverloadedRecordDot #-}

{-# OPTIONS_GHC -Wno-unused-imports #-}
  -- FIXME!

module Language.LR.ExampleICC where

-- base pkgs:
import           Data.Word

-- local modules:
import           Language.LR.API
import           Language.PEAR.Types
import           Language.PEAR.Util
import qualified Language.PEAR.Region.API as R -- region
import           Language.PEAR.Region.API(Region(..))

---- ICC -----------------------------------------------------------
icc :: Monad m => Region -> PT m [TED]
icc rFile =
  do
  (cnt,rfoCnt) <- pInt4Bytes                    @! rFile
  (tbl,     _) <- pManySRPs (val cnt) pTblEntry @! rfoCnt
  rsTeds       <- except
                $ mapM (getSubRegion rFile) (val tbl)
  crsFile      <- makeCanonicalRegions
                    (rgn cnt : rgn tbl : rsTeds)
  isCavityFree <- hasNoCavities $ R.complementCRs rFile crsFile
  teds         <- mapM applyPTED rsTeds
  teds_safe    <- if isCavityFree
                  then return teds
                  else throwE ["teds not safe"]

  return teds_safe

  where
  pTblEntry = pTwoWord32s

---- syntactical things --------------------------------------------
-- FIXME: syntax
--   use 'x.v' and 'x.r' syntax with your 'special pairs'
-- for now:
val = fst
rgn = snd

makeCanonicalRegions xs = except $ R.regionsDisjoint_Possibly xs

hasNoCavities _cr = return True
  -- FIXME!
  -- see pear/src/Language/OptimalPEAR/Examples/ICC_Optimal.hs

---- TED parsing ---------------------------------------------------

newtype TED = TED String

-- | very adhoc, just nab the string contained in the whole region:
applyPTED r = pTED (r_width r) `appSRP` r
  where
  pTED :: Monad m => Width -> SRP m TED
  pTED w = mkPrimSRP w (Right . TED)

getSubRegion r (loc,sz) = R.subRegionP r (toLoc loc) (toLoc sz)
  -- note: different calls could overlap.
  -- note: we might check here that all fall into right section


---- The optimal 'inspiration --------------------------------------
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


---- libraries -----------------------------------------------------

-- FIXME: these are demo-friendly but WRONG:

pInt4Bytes :: Monad m => SRP m Int
pInt4Bytes = mkPrimSRP 4  (Right . (read :: String -> Int))

pWord32 :: Monad m => SRP m Word32
pWord32 = mkPrimSRP 4 (Right . (read :: String -> Word32))

pTwoWord32s :: Monad m => SRP m (Word32,Word32)
pTwoWord32s = pairSRPs pWord32 pWord32
