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
  , split1P
  , split1F
  , split1_Possibly
  , splitWidths
  , splitWidthsP  -- FIXME[C1]: make naming consistent
  , splitWidths'
  , splitNWidthP
  , splitNWidthExact

  , rTake
  , rDrop

  , subRegion_Possibly
  , subRegionP
  , regionMinusSuffix
  , regionsDisjointP
  , regionsDisjoint_Possibly
  , regionsDisjoint_Possibly_
  , regionsOverlap

  , ppRegion
  , complementCRs
  , getContext
  )
where  

import Language.PEAR.Region.Implem

split1P          = Language.PEAR.Region.Implem.split1_Possibly
split1F          = Language.PEAR.Region.Implem.split1
subRegionP       = Language.PEAR.Region.Implem.subRegion_Possibly
regionsDisjointP = Language.PEAR.Region.Implem.regionsDisjoint_Possibly
