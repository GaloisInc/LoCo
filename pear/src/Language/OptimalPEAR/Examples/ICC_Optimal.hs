{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# OPTIONS_GHC -ddump-splices #-}

{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}
  -- FIXME!

module Language.OptimalPEAR.Examples.ICC_Optimal where

-- base pkgs:
import           Control.Monad
import           Data.IORef
import           Data.Word
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Except
import           Data.Word (Word64, Word8)
import           System.IO.Unsafe (unsafePerformIO)

-- package locc (optimal):
import           Language.Optimal.Quote (optimal)
import           Thunk.RefVal (Thunked, delayAction, force)

-- local modules:
import           Language.PEAR.Primitives
import           Language.PEAR.ParserLibrary
import           Language.PEAR.Region.API (Region,r_width,CanonicalRegions)
import qualified Language.PEAR.Region.API as R
import           Language.PEAR.Types
import           Language.PEAR.Util
import           Language.OptimalPEAR.Examples.ICC_V1 hiding (icc)


---- Example: ICC (in Optimal) -------------------------------------

type TBLR = ( TBL , Region )
type TBL  = [(Word32,Word32)]

[optimal|
type ICC = { cnt        : Word32
           , teds       : [TED]
           , teds_safe  : [TED]
           , cavityFree : Bool
           
           , cnt_r    : Region
           , r2       : Region
           , rs       : [Region]
           , tblR3    : TBLR
           , tbl      : TBL
           , r3       : Region
           , teds_rs  : [Region]
           , canon_rs : CanonicalRegions
           , r0       : Region
           }

icc : ICC
icc =
  { cnt        = <| pWord32 `app` cnt_r |>,
    tbl        = <| return (fst tblR3) |>,
    
    teds       = <| teds' teds_rs |>,
    teds_safe  = <| if cavityFree then return teds
                                  else throwE' ["teds not safe"]
                 |>, 
    teds_rs    = <| teds_rs' tbl r0 |>,

    cavityFree = <| cavityFree' canon_rs r0 |>,
    canon_rs   = <| canon_rs' cnt_r r2 r3 teds_rs |>,
    cnt_r      = <| return (rs!!0) |>,
    r2         = <| return (rs!!1) |>,
    rs         = <| failP $ unPair <$> R.split1P r0 wWord32 |>,
    
    tblR3      = <| tblR3' rs cnt |>,
    r3         = <| return (snd tblR3) |>,
    r0         = <| liftIO $ getTopLevelRegion |>
  }
|]

icc :: MonadIO m => FailT m (ICC (FailT m))

cavityFree' canon_rs r0 =
  case cavities of
    R.CR [] -> return True
    R.CR rs -> do
               -- NOTE: only side-effect in the ICC spec!
               --  - rather fail?
               liftIO $ mapM_ putStrLn
                      $ ["Cavities present:"
                        , ""
                        ] ++ map ((" "++) . R.ppRegion) rs
               return False
  where
  cavities = r0 `R.complementCRs` canon_rs

-- | create region set (in canonical form), fail if not disjoint.
canon_rs' cnt_r r2 r3 teds_rs =
  failP
    $ elaboratePossibly ["TED values are not disjoint:"]
    $ R.regionsDisjointP (cnt_r : r_tbl : teds_rs)
 where
 r_tbl = r2 `R.regionMinusSuffix` r3

teds' teds_rs =
  forM teds_rs $ \r-> app (pTED_FxdWd_NoFlT (r_width r)) r
 
teds_rs' tbl r0 =
  failP $
    (forM tbl $
      \(loc,sz)-> R.subRegionP r0 (toLoc loc) (toLoc sz))
                  
tblR3' :: forall m. MonadIO m => [Region] -> Word32 -> FailT m TBLR
tblR3' [_,r2] cnt =
  do
  let
    lpTbl :: RgnPrsr_FxdWd_NoFlT m [(Word32, Word32)]
      -- FIXME: eliminate need for signature
    lpTbl = pMany_FxdWd_NoFlT (fromIntegral cnt) pTwoWords_FxdWd_NoFlT
  (r_tbl, r3) <- failP $ R.split1P r2 (width_FxdWd lpTbl)
  tbl  <- lpTbl `app` r_tbl 
  return (tbl,r3)
    

---- Hack ----------------------------------------------------------
-- FIXME: this a hack until we have parameterized modules.

regionGlobalVar :: IORef (Maybe Region)
{-# NOINLINE regionGlobalVar #-}
regionGlobalVar = unsafePerformIO (newIORef Nothing)

setTopLevelRegion :: MonadIO m => Region -> m ()
setTopLevelRegion r =
  do
  liftIO $ writeIORef regionGlobalVar (Just r)

getTopLevelRegion :: MonadIO m => m Region
getTopLevelRegion =
  do
  mr <- liftIO $ readIORef regionGlobalVar
  case mr of
    Just x -> return x
    _      -> error "getTopLevelRegion"




