{-# LANGUAGE OverloadedRecordDot #-}

module Language.OptimalPEAR.Examples.ICC_Spec where

-- base pkgs:
import           Data.Word
import           Text.Read

-- local modules:
import           Language.PEAR.API
import           Language.PEAR.Util
import qualified Language.PEAR.Region.API as R -- region
import           Language.PEAR.Region.API(Region(..))


---- ICC (PEAR version) --------------------------------------------

-- icc :: Monad m => Region -> PT m (VR Int)
-- icc :: Monad m => Region -> PT m (VR [(Word32, Word32)])
-- icc :: Monad m => Region -> PT m [TED]
icc_pear rFile =
  do
  (cnt,rRest)  <- pInt4Bytes                  @!  rFile
  tbl          <- pManySRPs (cnt.v) pTblEntry @!- rRest
  rsTeds       <- except $ mapM (getSubRegion rFile) tbl.v
  teds         <- mapM applyPTED rsTeds
  crsFile      <- makeCanonicalRegions (cnt.r : tbl.r : rsTeds)
  isCavityFree <- hasNoCavities $ R.complementCRs rFile crsFile
  teds_safe    <- if isCavityFree
                  then return teds
                  else throwE ["teds not safe"]
  -- return teds
  return teds_safe

pTblEntry :: Monad m => SRP m (Word32,Word32)
pTblEntry = pTwoWord32s

---- Syntactical & UI things ---------------------------------------

-- For more 'user friendly' variations, see
--    pear/src/Language/OptimalPEAR/Examples/ICC_Optimal.hs

makeCanonicalRegions :: Monad m => [Region] -> PT m R.CanonicalRegions
makeCanonicalRegions xs = except $ R.regionsDisjoint_Possibly xs

hasNoCavities :: Monad m => R.CanonicalRegions -> m Bool
hasNoCavities cavities =
  case cavities of
    R.CR [] -> return True
    R.CR _  -> return False
               {-
               -- NOTE: only side-effect in icc
               --  - rather fail?
               liftIO $ mapM_ putStrLn
                      $ ["Cavities present:"
                        , ""
                        ] ++ map ((" "++) . R.ppRegion) rs
               -}

---- TED parsing ---------------------------------------------------

newtype TED = TED String
              deriving (Eq,Ord,Read,Show)

-- | very adhoc, just nab the string contained in the whole region:
applyPTED :: Monad m => Region -> PT m TED
applyPTED r = v <$> pTED (r_width r) `appSRP` r
  where
  pTED :: Monad m => Width -> SRP m TED
  pTED w = mkPrimSRP w (Right . TED)

getSubRegion :: Integral a => Region -> (a,a) -> Possibly Region
getSubRegion r (loc,sz) = R.subRegionP r (toLoc loc) (toLoc sz)
  -- NOTE:
  --  - different calls could overlap.
  --  - we might check *here* that all fall into right section
  -- Hmmm:
  --  - more overloading?? have another Integral type?
  --  - no overloading & need 'toLoc' to convert from Word32 to Word64


---- demo friendly parsing library ---------------------------------

readP :: Read a => String -> Possibly a
readP s = case readEither s of
            Left msg -> Left [msg]
            Right a  -> Right a

-- CAVEAT: these are demo-friendly (easy to write & view) but WRONG:

pInt4Bytes :: Monad m => SRP m Int
pInt4Bytes = mkPrimSRP 4 readP

pWord32 :: Monad m => SRP m Word32
pWord32 = mkPrimSRP 4 readP

pTwoWord32s :: Monad m => SRP m (Word32,Word32)
pTwoWord32s = pairSRPs pWord32 pWord32
