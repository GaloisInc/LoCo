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
import           Language.OptimalPEAR.Examples.ICC_V1


-- Old NOTEs
--   - a conversion of **.ICCV2 into MEP form.
--     - see **.ICCV2 for notes/TODOs/improvements!
--     - hmmm: import parts of that here?
--     - you are capturing exceptions with PrimFail, not with FailT monad.


---- Example: ICC (in Optimal) -------------------------------------

type TBLR = ([ (Word32,Word32) ] , Region )
  
[optimal|
type ICC = { rs    : [Region]
           , rCnt  : Region
           , r2    : Region

           , cnt   : Word32
           , tbl_r : TBLR
           }

icc2 : ICC
icc2 =
  { 
  rs    = <| rs' |>,
  rCnt  = <| return (rs!!0) |>,
  r2    = <| return (rs!!1) |>,
  cnt   = <| pWord32 `app` rCnt |>,
  tbl_r = <| tbl_r' rs cnt |>
  }
|]

icc2 :: MonadIO m => FailT m (ICC (FailT m))

{-
A couple locc/Optimal issues:

 FIXME: parsing error when I try to inline type synonym TBLR inside optimal.

 FIXME: when cntRs' inlined, get
     Exception when trying to run compile-time code:
       TODO: finish constructors in `expFreeVars`
         (failed on DoE Nothing [BindS (TupP [VarP...)
-}


tbl_r' :: forall m. MonadIO m => [Region] -> Word32 -> FailT m TBLR
tbl_r' [_,r2] cnt =
  do
  let
    lpTbl :: RgnPrsr_FxdWd_NoFlT m [(Word32, Word32)]
      -- FIXME: why do you need sig?
    lpTbl = pMany_FxdWd_NoFlT (fromIntegral cnt) pTwoWords_FxdWd_NoFlT
  case R.split1_Possibly r2 (width_FxdWd lpTbl) of
         Right (r_tbl, r3) ->
             do
             tbl  <- lpTbl `app` r_tbl 
             return (tbl,r3)
         Left ms ->
             throwE' ms
  
rs' =
  let
    wCnt = width_FxdWd pWord32_FxdWd_NoFl
  in
    do
    r0 <- liftIO $ getTopLevelRegion
    (r1,r2) <- except' $ R.split1_Possibly r0 wCnt
    return [r1,r2]

  
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
          
