{-# LANGUAGE LambdaCase #-}
module Language.PEAR.Region.Implem where

-- base pkgs:
import Control.Exception(assert)
import Control.Monad
import Data.List

-- local modules:
import Language.PEAR.Util


---- Types -------------------------------------------------------------------

-- | A region (range) of the input file.
--
-- This is intended to be an abstract type!
data Region = R { r_start :: Loc
                , r_width :: Width
                }
              deriving (Eq,Ord,Read)
              -- NOTE: r_width can be 0

instance Show Region where
  show (R a b) = unwords[ "R", show a, show b]

width :: Region -> Width
width = r_width

ppRegion :: Region -> String
ppRegion (R s w) = concat [show s,"..(",show w,")..",show (s+w-1)]
  
-- private functions:

-- r_top is the location 1 beyond the last byte in region
-- - abstract; not for export
r_top :: Region -> Loc
r_top (R s w) = s+w


---- Canonical Region Lists --------------------------------------------------

-- FIXME: A better 'algebra'?
--  - the disallowing of overlaps makes it all more awkward.
-- Many properties, capture!

-- canonical : must be ordered, 'adjacents' merged, & no empty regions
newtype CanonicalRegions = CR [Region]
                           deriving (Eq,Ord,Read,Show)

regionsDisjoint_Possibly :: [Region] -> Possibly CanonicalRegions
regionsDisjoint_Possibly rs =
  foldM (flip addNonOverlappingRegion) (CR []) rs

regionsDisjoint_Possibly_ :: [Region] -> Possibly ()
regionsDisjoint_Possibly_ rs =
  const () <$> regionsDisjoint_Possibly rs


-- | add Region to CanonicalRegions: fail if overlap
addNonOverlappingRegion ::
  Region -> CanonicalRegions -> Possibly CanonicalRegions
addNonOverlappingRegion =
  curry $ \case
    (R _ 0  , x         ) -> Right x
    (r      , CR []     ) -> Right $ CR [r]
    (r1     , CR (r2:rs)) ->
        if s1 <= s2 then
          -- r1 will either start or cause failure
          case compare (s1+w1) s2 of
            LT -> Right $ CR (r1:r2:rs)
            EQ -> Right $ CR (R s1 (w1+w2) : rs)
            GT -> Left $ [unwords [show r1,"overlaps",show r2]]
        else 
          -- r1 may belong anywhere in rs (or cause failure)
          case compare s1 (s2+w2) of 
            LT -> Left [unwords [show r1,"overlaps",show r2]]
            EQ -> Right $ CR (R s2 (w1+w2) : rs)
            GT -> do
                  (CR rs') <- addNonOverlappingRegion r1 (CR rs)
                  return $ CR (r2:rs')
      where
      R s1 w1 = r1
      R s2 w2 = r2



-- | complementCRs - ...
-- 
-- PRECONDITION:
--   r should contain all the regions in cr
complementCRs :: Region -> CanonicalRegions -> CanonicalRegions
complementCRs r (CR rs) =
  CR $ filter (\r''-> width r'' /= 0) (rs'++[r'])
  where
  (r',rs') = mapAccumL (\r1 r2 -> swap $ getContext r1 r2) r rs
  swap (a,b) = (b,a)
  

-- | getContext r1 r2 = (ra,rb)  when r1 == ra <> r2 <> rb
getContext :: Region -> Region -> (Region,Region)
getContext r1@(R s1 _w1) r2@(R s2 w2) =
  if regionContains r1 r2 then
    ( R s1 (s2-s1)
    , R (s2+w2) (r_top r1 - r_top r2)
    )
  else
    error $ unwords ["getContext: not regionContains",show r1,show r2]
  

---- Region primitives -------------------------------------------------------

-- a better abstraction? (define in terms of 'getContext'?)

regionMinusSuffix :: Region -> Region -> Region
regionMinusSuffix r1@(R s1 w1) r2@(R s2 w2) =
  assert (r_top r1 == r_top r2 && s1 < s2) $ 
  R s1 (w1-w2)
  
regionPrecedes :: Region -> Region -> Bool
regionPrecedes (R s1 w1) (R s2 _w2) = s1 + w1 <= s2
  
regionsOverlap :: Region -> Region -> Bool
regionsOverlap r1 r2 = not (regionPrecedes r1 r2 || regionPrecedes r2 r1)
  -- FIXME[E3]?

-- regionContains r1 r2 -- region r1 contains r2
regionContains :: Region -> Region -> Bool
regionContains (R s1 w1) (R s2 w2) =
  s1 <= s2 && s2+w2 <= s1+w1
  
subRegion_Possibly :: Region -> Loc -> Width -> Possibly Region
subRegion_Possibly (R st w) offset w' =
  if offset + w' <= w then
    Right (R (st+offset) w')
  else
    Left $ [unwords [ "no subregion"
                    , show (offset, w')
                    , "of region"
                    , show (R st w)
                    ]]

split1 :: Region -> Width -> (Region,Region)
split1 r w = case split1_Possibly r w of
               Left s  -> error $ unlines s
               Right x -> x

split1_Possibly :: Region -> Width -> Possibly (Region,Region)
split1_Possibly (R s w) w' =
  if w' <= w then
    Right $ (R s w', R (s+w') (w - w'))
  else
    Left $ [unwords [ "no split1 of region"
                    , show (R s w)
                    , "into width"
                    , show w'
                    ]]

-- | n widths gives n regions
splitWidthsP :: Region -> [Width] -> Possibly [Region]
splitWidthsP r' ws' = splitWidthsP' r' ws'

  where                    
  splitWidthsP' r []
    | width r == 0 = Right []
    | otherwise    = Left [ unwords ["cannot splitWidth region"
                                    , show r'
                                    , "into widths"
                                    , show ws'
                                    ]]
  splitWidthsP' r (w:ws)
                   = do
                     (r1,r2) <- split1_Possibly r w
                     (r1:) <$> splitWidthsP' r2 ws

-- | n widths gives n regions
splitWidths :: Region -> [Width] -> [Region]
splitWidths r [] | width r == 0 = []
                 | otherwise    = error "splitWidths"
splitWidths r (w:ws) = let (r1,r2) = split1 r w
                       in  r1 : splitWidths r2 ws

-- | n widths gives n+1 regions
splitWidths' :: Region -> [Width] -> [Region]  -- last region gets rmng
splitWidths' r [] | width r == 0 = error "splitWidths' expecting remainder"
                  | otherwise    = [r]
splitWidths' r (w:ws) = let (r1,r2) = split1 r w
                       in  r1 : splitWidths' r2 ws

