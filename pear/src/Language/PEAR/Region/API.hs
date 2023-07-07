{-# LANGUAGE LambdaCase #-}
module Language.PEAR.Region.API
  (
  -- | abstract:
    Region (..)   -- FIXME: this MUST be abstract!
                  -- (leaving thus for convenience)
  -- | hmmm:
  , CanonicalRegions (..)
    
  -- | region primitives
  , regionContains
  , split1
  , split1_Possibly
  , splitWidths
  , splitWidthsP  -- FIXME: make naming consistent
  , splitWidths'
  , subRegion_Possibly
  , regionMinusSuffix
  , regionsDisjoint_Possibly
  , regionsDisjoint_Possibly_
  , regionsOverlap

  , ppRegion
  , complementCRs
  , getContext
  )
where  

import Language.PEAR.Region.Implem

