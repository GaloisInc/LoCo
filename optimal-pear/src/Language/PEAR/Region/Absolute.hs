module Language.PEAR.Region.Absolute where

import Language.PEAR.API (Contents, DRP, PT, SRP, VR (..), appSRP, except, runPT)
import Language.PEAR.API qualified as PEAR
import Language.PEAR.Region.API qualified as Region
import Language.PEAR.Util (Loc, Possibly, toLoc)

{-
Motivation: I think it might be useful to have a `Region` abstraction that keeps
track of a region's position in the original document, so that byte-offset
resolution can happen with access to only the region of the pointed-to data,
rather than the top-level region. E.g., in ICC, where all the tag elements
occupy a contiguous range of bytes, one can see more clearly that the elements
rely on that block when the top-level region doesn't need to be passed in solely
for the sake of doing offset arithmetic.

To be clear, this module provides no new functionality - it *can* be accomplished
in-PEAR/in-Optimal, which may be preferable for explicitness' sake.
-}

data Region = Region {rAbsoluteOffset :: Loc, rPEARRegion :: Region.Region}
  deriving (Show)

applyToContents :: (Region -> PT m a) -> Contents -> m (Possibly a)
applyToContents parse contents = runPT (parse globalRegion) contents
  where
    globalRegion = Region 0 (Region.R 0 (toLoc (length contents)))

(@$$) :: Monad m => SRP m a -> Region -> PT m a
(@$$) srp Region {..} =
  do
    VR val _ <- appSRP srp rPEARRegion
    pure val

appDRP' :: Monad m => DRP m a -> Region -> PT m (a, Region)
appDRP' drp Region {..} =
  do
    (res, newRegion) <- drp `PEAR.appDRP'` rPEARRegion
    -- TODO: unpack `newRegion` to see how we need to update our offset
    pure (res, undefined)

-- | Slice (take/drop/both) relative to this region's offset and width
slice ::
  (Integral a, Monad m) =>
  Region ->
  Maybe a ->
  Maybe a ->
  PT m Region
slice Region {..} beginM endM =
  case (beginM, endM) of
    (Nothing, Nothing) -> pure Region {..}
    (Just begin, Nothing) ->
      do
        let newOffset = rAbsoluteOffset + fromIntegral begin
        newRegion <- except (Region.rDrop (fromIntegral begin) rPEARRegion)
        pure Region {rAbsoluteOffset = newOffset, rPEARRegion = newRegion}
    (Nothing, Just end) ->
      do
        newRegion <- except (Region.rTake (fromIntegral end) rPEARRegion)
        pure Region {rPEARRegion = newRegion, ..}
    (Just begin, Just end) ->
      do
        newRegion <-
          except $
            Region.rTake (fromIntegral end) rPEARRegion >>= Region.rDrop (fromIntegral begin)
        let newOffset = rAbsoluteOffset + fromIntegral begin
        pure Region {rAbsoluteOffset = newOffset, rPEARRegion = newRegion}

-- | Slice (take/drop/both) relative to the absolute (in terms of top-level
-- contents under parse) offset and width
sliceAbs ::
  (Integral offset, Monad m) =>
  Region ->
  Maybe offset ->
  Maybe offset ->
  PT m Region
sliceAbs Region {..} beginM endM =
  slice
    Region {..}
    ((\begin -> fromIntegral begin - rAbsoluteOffset) <$> beginM)
    ((\end -> fromIntegral end - rAbsoluteOffset) <$> endM)
