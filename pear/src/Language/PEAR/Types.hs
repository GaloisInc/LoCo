{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}

module Language.PEAR.Types where

-- base pkgs
import           GHC.Generics                      (Generic)

-- QuickCheck & generic-arbitrary pkgs:
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic

-- local modules:
import           Language.PEAR.Util


---- Syntax --------------------------------------------------------

infixr 0 -->

s --> a = (s,a)

---- WidthConstraints ----------------------------------------------

data MaxWidth =
    MW Width    -- FIXME: change name!
  | MW_NoMax    -- this order matters: for Ord!
  deriving (Eq,Ord,Read,Show,Generic)
  deriving Arbitrary via (GenericArbitrary MaxWidth)
              
type WC = WidthConstraint

data WidthConstraint =
  WC { minWidth :: Offset, maxWidth :: MaxWidth}
  deriving (Eq,Read,Show,Generic)
  deriving Arbitrary via (GenericArbitrary WidthConstraint)

  -- invariant:
  --  forall x : WidthConstraint.
  --    maxWidth x == MW w  ==>  minWidth x <= w

  -- FIXME[R2]: create smart constructor enforcing this.


checkWC :: WidthConstraint -> Width -> Bool
checkWC (WC min' mmw) w | w < min'  = False
                        | otherwise = case mmw of
                                       MW_NoMax -> True                      
                                       MW mw    -> w <= mw

addMaxWidth (MW x) (MW y) = MW (x+y)
addMaxWidth _      _      = MW_NoMax
    
widthToWC :: Width -> WidthConstraint
widthToWC n = WC n (MW n)

dynWC :: WidthConstraint
dynWC = WC 0 MW_NoMax

---------------------------------------------------------------

-- | MaxWidth and WidthConstraint are Monoids where <> corresponds to
--   juxtaposition/addition of widths

instance Semigroup MaxWidth where
  (<>) = addMaxWidth
  
instance Semigroup WidthConstraint where
  WC min1 max1 <> WC min2 max2 = WC (min1+min2) (max1 <> max2)

instance Monoid MaxWidth where
  mempty = MW 0
  
instance Monoid WidthConstraint where
  mempty = WC 0 mempty


---- deprecated: -------------------------------------------------------------

sequenceWCs :: [WidthConstraint] -> WidthConstraint
sequenceWCs wcs =
  WC
    (sum $ map minWidth wcs)
    (foldr addMaxWidth (MW 0) (map maxWidth wcs))

prop_WidthConstraint1 :: [WidthConstraint] -> Bool
prop_WidthConstraint1 cs = sequenceWCs cs == mconcat cs

