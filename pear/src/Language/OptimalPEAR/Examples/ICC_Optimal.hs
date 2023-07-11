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
import           Language.PEAR.Region.API (Region,r_width)
import qualified Language.PEAR.Region.API as R
import           Language.PEAR.Types
import           Language.PEAR.Util
import           Language.OptimalPEAR.Examples.ICC_V1 hiding (icc)


---- Example: ICC (in Optimal) -------------------------------------

type TBLR = ( TBL , Region )
type TBL  = [(Word32,Word32)]
  
[optimal|
type ICC = { cnt     : Word32
           , teds    : [TED]

           , cnt_r   : Region
           , r2      : Region
           , rs      : [Region]
           , tblR3   : TBLR
           , tbl     : TBL
           , r3      : Region
           , teds_rs : [Region]
           }

icc : ICC
icc =
  { cnt     = <| pWord32 `app` cnt_r |>,
    tbl     = <| return (fst tblR3) |>,

    teds    = <| teds' teds_rs |>,
    teds_rs = <| teds_rs' tbl |>,

    cnt_r   = <| return (rs!!0) |>,
    r2      = <| return (rs!!1) |>,
    rs      = <| rs' |>,

    tblR3   = <| tblR3' rs cnt |>,
    r3      = <| return (snd tblR3) |>
  }
|]

icc :: MonadIO m => FailT m (ICC (FailT m))

rs' =
  let
    wCnt = width_FxdWd pWord32_FxdWd_NoFl
  in
    do
    r0 <- liftIO $ getTopLevelRegion
    (r1,r2) <- except' $ R.split1_Possibly r0 wCnt
    return [r1,r2]

teds' teds_rs =
  forM teds_rs $ \r-> app (pTED_FxdWd_NoFlT (r_width r)) r
 
teds_rs' tbl =
  do
  r0 <- liftIO $ getTopLevelRegion
  except' $
    (forM tbl $
      \(loc,sz)-> R.subRegion_Possibly r0 (toLoc loc) (toLoc sz))
                  
tblR3' :: forall m. MonadIO m => [Region] -> Word32 -> FailT m TBLR
tblR3' [_,r2] cnt =
  do
  let
    lpTbl :: RgnPrsr_FxdWd_NoFlT m [(Word32, Word32)]
      -- FIXME: eliminate need for signature
    lpTbl = pMany_FxdWd_NoFlT (fromIntegral cnt) pTwoWords_FxdWd_NoFlT
  case R.split1_Possibly r2 (width_FxdWd lpTbl) of
         Right (r_tbl, r3) ->
             do
             tbl  <- lpTbl `app` r_tbl 
             return (tbl,r3)
         Left ms ->
             throwE' ms
  
  
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


---- Issues --------------------------------------------------------
{-
A couple locc/Optimal issues:

 FIXME: parsing error when I try to inline type synonym TBLR inside optimal.

 FIXME: type synonym TBL_R causes optimal parsing error.

 FIXME: when cntRs' inlined, get
     Exception when trying to run compile-time code:
       TODO: finish constructors in `expFreeVars`
         (failed on DoE Nothing [BindS (TupP [VarP...)

 FIXME:
   when I define module element that isn't in type, we get warning, not error:

    src/Language/OptimalPEAR/Examples/ICC_Optimal.hs:(41,10)-(66,2): 
     warning: [-Wunused-matches]
        Defined but not used: ‘teds_rs’
       |
    41 | [optimal|
       |          ^...

   hmmm: other behavior?

-}


