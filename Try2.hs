{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}

module Try2 where

import Control.Exception (assert)
import Control.Arrow
import Control.Applicative
import Data.Functor


{-
TERMS:
 - M-E-Ps
 - Regions (not ranges, not _)
 - Locations (not offsets)
-}


---- Types -------------------------------------------------------------------

data Value = Prim Int
           | Prod [Value]
           | Sum Int Value
           deriving (Eq,Ord,Read,Show)

-- | file locations (and widths too): unsigned.
type Loc = Int -- FIXME

-- | Regions : as yet to be fully determined (not fully constrained)
data Region = R_Dyn  {r_start :: Loc}
            | R_MaxW {r_start :: Loc, r_maxWidth :: Loc}
            | R_FixW {r_start :: Loc, r_width    :: Loc}
            deriving (Eq,Ord,Read,Show)
   -- FIXMEs:
   --  - better names for Region & RRegion to distinguish better!

   --  - merge last two into a min & max width?

   --  - MaxW:
   --     - Also have parsers with MaxW ..., so?
   --     - E.g., int32, int64

---- Result Regions --------------------------------------------------------

-- | RRs - Result Regions: represents what parts of the file were parsed.
type RRs = [RRegion]


-- | Attaches 'RRs' to 'a'.
data Ann a = Ann a [RRegion]
             deriving (Eq,Ord,Read,Show,Functor)

-- | note recursive pair here.
data RRegion = RR { rr_start :: RLoc
                  , rr_width :: RLoc
                  }
               deriving (Eq,Ord,Read,Show)

-- | Result Location, however, we need to be able to capture two things:
--    1. where something was parsed, R_Int 
--    2. where we parsed the "pointers, offsets, and lengths" required to
--       know where to parse.

data RLoc = RL_Int Loc
             -- ^ constant Loc value for 
          | RL_Indrct [RRegion] RLoc  
             -- ^ needed to parse file to get val
             -- could be Ptr
             -- not recording the interpretation of bytes to get val
          deriving (Eq,Ord,Read,Show)

width_rr :: RRegion -> Loc
width_rr r = stub

splitAt_rr :: Loc -> RRegion -> [RRegion]
splitAt_rr l r = stub l r


---- Streams --------------------------------------------------------------

-- Stream
--   seek-able
--   constant access time
--   (might be representable as an Start/End Int pair on a file)


type Stream = String -- FIXME: String is NOT a good Stream!

-- | stream operations
--
-- Might we have a set of operations that are "linear" on the Stream data?
-- E.g., splitAt, etc.

width_s :: Stream -> Loc
width_s = stub

regionOf_s :: Stream -> RRegion
regionOf_s s = RR { rr_start= RL_Int 0
                  , rr_width= RL_Int (width_s s)
                  }

getRegion  :: RRegion -> Stream -> Stream
getRegion r s = stub r s
  -- FIXME: better name.
  

---- Parsers --------------------------------------------------------------

{-
- Design
  - Use Region (R_Dyn | R_MaxW | R_FixW) to merge ParserFW/ParserDyn ?

- Harder Design
  - How easily can we generalize this to M-E-P parsers?

- Later Design
  - Function to analyze & convert from DynP to FWP (when it is).
  - Capture whether a parser can fail (or must succeed)
-}

-- | A Fixed Width parser: it fails unless we consume the full stream.
-- 
-- Int is width of stream, we want to capture knowing this 'statically
-- No 'Ann' is required as all substructure is now gone.
-- 
-- Issues/Questions:
--  - FUTURE: changed for MEP?

data FWParser a = FWP { width_fwp :: Int
                      , parser    :: Stream -> Result a
                      }
     deriving (Functor)

-- | A Dynamic Parser: it may jump around, we don't know the length, it may
--   not fully consume the Stream.
-- 
-- The 'RRegion' returned is the region of the Stream remaining/unparsed.
--   - could this be computed from 'Ann a'?
--   - Although 'Ann a' may contain many jumped-to regions.

data DynParser a = DynP (Stream -> Result (Ann a, RRegion))
     deriving (Functor)
     -- FIXME: Q. want Loc when fail?

-- | the Result of a Parser, with errors.
data Result a = Result a                -- ^ successful parse result
              | Error_Short             -- ^ parser did not reach region end
              | Error_Parse [String]    -- ^ errors in parser itself 
              deriving (Eq,Ord,Read,Show,Functor)

instance Applicative Result where
  pure a = Result a
  
  liftA2 f (Result a)       (Result b) = Result (f a b)
  liftA2 _ Error_Short      _          = Error_Short
  liftA2 _ (Error_Parse ss) _          = Error_Parse ss
    
instance Monad Result where
  x >>= f = case x of
              Result a       -> f a
              Error_Short    -> Error_Short
              Error_Parse ss -> Error_Parse ss
              
---- applying parsers --------------------------------------------------------

-- Design
--  - use a reader monad to hide the full/restricted stream?

-- | apply: the 'Stream' is already restricted to the relevant Region.

applyDynParser :: Stream -> DynParser a -> Result (Ann a, RRegion)
applyFWParser  :: Stream -> FWParser a  -> Result a

applyDynParser = stub
applyFWParser  = stub



{- alts
  A. dynParser to FW : parse and also verify at EOF
  B dynParser to Dyn: parse, and return remaining
  C. fwParser to  FW:  [statically] verify length, parse
    - lengths [in type system] must be right
  D. fwParser to  Dyn: [statically] return remaining, if any
    - if Dyn > len then can split statically: becomes like fwParser to FW
-}

{-
applyParser :: Stream -> DynParser a -> Maybe (a, DynRegion)
applyParser s (DynP p) = p s

applyFWParser  :: Int -> Stream -> FWParser  a -> Maybe a
applyFWParser i s (FWP j p) = assert (j == length s)
                            $ p s

  -- i == j?
  -- where to extract the 'j' width stream:
  --   - right here or in caller!!
  -- design is to extract out a fixed width stream! (rather than "i s")?
  --  -- use bytestreams!!
-}

---- examples: abstractions -----------------------------------------------

pXY :: DynParser (X,Y)
pXY = DynP p
  where
  p s = do
        x             <- applyFWParser  (getRegion r1 s) pX 
        (Ann y as,rr) <- applyDynParser (getRegion r2 s) pY
        return (Ann (x,y) (r1:r2:as), rr)
        where
        n = width_fwp pX
        [r1,r2] = splitAt_rr n (regionOf_s s)
                               


---- examples: basic parsers ----------------------------------------------

data X = X Int32
         deriving (Eq,Ord,Read,Show)

data Y = Y Int
         deriving (Eq,Ord,Read,Show)

pX :: FWParser X
pX = X <$> pInt32 -- static  

pY :: DynParser Y
pY = Y <$> pInt   -- dynamic

-- FIXME: TODO
-- pPY :: Parser (R Y)
-- pPY =  stub

  -- AHA: This parses 4-bytes, then jumps to a dynamic location, so ...
  --   - static parser (that calls dynamic)
  --   - dynamic parser??
  --   - MERGE these.
  
{-
  pPY :: [Region] -> Parser (R Y)
  pPY [r] = do
            offset <- pUint32 r
            rNew   <- mkReg offset
            y      <- pY rNew
            return (R rNew y)
-}

---- Prims -------------------------------------------------------------------

type Int32 = Int  -- FIXME

-- | binary, fixed width
pInt32 :: FWParser Int32 -- binary, 4 bytes!
pInt32 = FWP 4 $
         \s-> case readsM s of   -- FIXME: incorrect: this is 4 byte ascii
                Just (i,[]) -> Result i
                _           -> Error_Short -- leftover string is a fail.

-- | ascii, dynamic number of bytes
pInt :: DynParser Int
pInt = DynP $
         \s-> case readsM s of
                Just (i,s') -> Result ( Ann i [r1]
                                      , r2
                                      )
                               where
                               n = length s
                               [r1,r2] = splitAt_rr n (regionOf_s s)
                               
                Nothing     -> Error_Parse ["no Int parse"]
         -- FIXME: not efficient


---- Utils -------------------------------------------------------------------

stub = error "stub"

readM :: Read a => String -> Maybe a
readM s = case reads s of
             [(a,[])] -> Just a
             _        -> Nothing

readsM :: Read a => String -> Maybe (a, String)
readsM s = case reads s of
             [(a,s')] -> Just (a,s')
             _        -> Nothing

