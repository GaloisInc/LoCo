{-# LANGUAGE DeriveFunctor #-}

module Try2 where

import Control.Arrow
import Data.Functor


---- Types -------------------------------------------------------------------
data Value = Prim Int
           | Prod [Value]
           | Sum Int Value
       --  | Region R Value
           deriving (Eq,Ord,Read,Show)

data R a = Region Rng a
           deriving (Eq,Ord,Read,Show)

data Rng = Rng Int Int                -- FIXME
           deriving (Eq,Ord,Read,Show)

---- Low Level Primitives ----------------------------------------------------

-- Assume you have seek-able, constant access time stream,
type Stream = String -- FIXME: which String is not


data DynRange = DynRange Int      -- start only 
data FWRange  = FWRange  Int Int  -- fixed width

data MLRange  = MLRange  Int Int  -- max length range

  -- FIXME: what about max length ranges??
  --   - Also have parsers with MaxLength
  --   - E.g., int32, int64


-- | fails unless we consume stream
newtype FWParser  a = FWP (Stream -> Maybe a)
                      deriving (Functor)


-- | need not consume stream
newtype DynParser a = DynP (Stream -> Maybe (a, DynRange))
                      deriving (Functor)

  -- if successful, returns the ptr where we ended, start of 'next' range in
  --   the stream
  -- a reader monad: as Stream doesn't change

-- stream operations

getRngSt  :: DynRange -> Stream -> Stream  -- gets to EOF
getRngSt = stub

getFWRange  :: FWRange    -> Stream -> Stream
getFWRange = stub


---- applying parsers --------------------------------------------------------

{- alts
  A. dynParser to FW : parse and also verify at EOF
  B dynParser to Dyn: parse, and return remaining
  C. fwParser to  FW:  [statically] verify length, parse
    - lengths [in type system] must be right
  D. fwParser to  Dyn: [statically] return remaining, if any
    - if Dyn > len then can split statically: becomes like fwParser to FW
-}

applyDynParser :: Stream -> DynParser a -> Maybe (a, DynRange)
applyDynParser s (DynP p) = p s

applyFWParser  :: Stream -> FWParser  a -> Maybe a
applyFWParser s (FWP p) = p s

                     
---- examples & testing ------------------------------------------------------

data X = X Int
         deriving (Eq,Ord,Read,Show)

data Y = Y Int
         deriving (Eq,Ord,Read,Show)

-- FIXME: add MEP2 later!

pX :: DynParser X
pX = X <$> pInt

pY :: DynParser Y
pY = Y <$> pInt

pPY :: DynParser (R Y)
pPY = stub


---- Prims -------------------------------------------------------------------

pInt :: DynParser Int
pInt = DynP $
         \s-> case readsM s of
                Just (i,s') -> Just (i, DynRange (length s - length s'))
                Nothing     -> Nothing 
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

{-
  pY  :: [Rng]  -> Parser Y
  pY [r] = stub

  pPY :: [Rng] -> Parser (R Y)
  pPY [r] = do
            offset <- pUint32 r
            rNew   <- mkReg offset
            y      <- pY rNew
            return (R rNew y)
-}
