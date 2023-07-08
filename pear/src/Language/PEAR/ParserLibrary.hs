module Language.PEAR.RegionParser.ParserLibrary where

-- base pkgs:
import Control.Exception(assert)
import Data.Word

-- local modules:
import Language.PEAR.RegionParser.Primitives
import Language.PEAR.RegionParser.Types
import Language.PEAR.Util
import Language.PEAR.Region.API


---- RegionParser Combinators ------------------------------------------------

pMany_FxdWd_Fail :: Int -> RgnPrsr_FxdWd_Fail a -> RgnPrsr_FxdWd_Fail [a]
pMany_FxdWd_Fail i p =
  assert (i >= 0) $
  ( fromIntegral i * widthSingle
  , \r-> do
         let rs = splitWidths r (replicate i widthSingle)
         mapM (app_FxdWd_Fail p) rs
  )
  where
  widthSingle = width_FxdWd p

pMany_FxdWd_NoFl :: Int -> RgnPrsr_FxdWd_NoFl a -> RgnPrsr_FxdWd_NoFl [a]
pMany_FxdWd_NoFl i p =
  assert (i >= 0) $
  ( fromIntegral i * widthSingle
  , \r-> do
         let rs = splitWidths r (replicate i widthSingle)
         mapM (app_FxdWd_NoFl p) rs
  )
  where
  widthSingle = width_FxdWd p

pMany_FxdWd_NoFlT
  :: Monad m => Int -> RgnPrsr_FxdWd_NoFlT m a -> RgnPrsr_FxdWd_NoFlT m [a]
pMany_FxdWd_NoFlT i p =
  assert (i >= 0) $
  ( fromIntegral i * widthSingle
  , \r-> do
         let rs = splitWidths r (replicate i widthSingle)
         mapM (app_FxdWd_NoFlT p) rs
  )
  where
  widthSingle = width_FxdWd p


---- FxdWd_Fail Word32 RgnPrsrs (pretending to be binary, but ascii) ------------------

-- These are not binary form!
-- These will be given 4 characters but will just use read.
-- useful for debugging and writing tests!
-- FIXME: in current form BOTH can fail


pWord32_FxdWd_Fail :: RgnPrsr_FxdWd_Fail Word32
pWord32_FxdWd_Fail = lift_FxdWd_NoFl pWord32_FxdWd_NoFl
                          

pWord32_FxdWd_NoFl :: RgnPrsr_FxdWd_NoFl Word32
pWord32_FxdWd_NoFl = mkPrim_FxdWd_NoFl 4 (read :: String -> Word32)

---- FIXME: NEW

pWord32_FxdWd_FailT :: Monad m => RgnPrsr_FxdWd_FailT m Word32
pWord32_FxdWd_FailT = lift_FxdWd_NoFlT pWord32_FxdWd_NoFlT
                          

pWord32_FxdWd_NoFlT :: Monad m => RgnPrsr_FxdWd_NoFlT m Word32
pWord32_FxdWd_NoFlT = mkPrim_FxdWd_NoFlT 4 (read :: String -> Word32)


---- Ascii Number RgnPrsrs ----------------------------------------------------


pWord32Ascii_DynWd_Fail :: RgnPrsr_DynWd_Fail Word32
pWord32Ascii_DynWd_Fail = mkPrim_DynWd_Fail
              (WC 1 (MW 10)) -- width would disallow leading zeros (TODO)
              readsPWidth 

pIntAscii_DynWd_Fail :: RgnPrsr_DynWd_Fail Int
pIntAscii_DynWd_Fail = mkPrim_DynWd_Fail
           (WC 1 (MW 100)) -- arbitrary maximum
           readsPWidth


