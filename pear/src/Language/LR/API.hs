{-# LANGUAGE OverloadedRecordDot #-}

module Language.LR.API
  ( module Language.LR.API
  , except
  , throwE
  )
where

-- base pkgs:
import           Data.List
import           Control.Exception (assert)

-- transformer pkg:
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except
-- import        Control.Monad.IO.Class
-- import        Data.Functor.Identity

-- FIXME: improve these to better fit the use here
import           Language.PEAR.Types
import           Language.PEAR.Util
import qualified Language.PEAR.Region.API as R -- region
import           Language.PEAR.Region.API(Region(..))

-- FIXME[R1]: Hide the abstraction!!
--   - make some types abstract
--   - let the "right functions" out, hide everything else
--   - but also export needed functions/types from Region/Etc

---- Types ---------------------------------------------------------

type Contents = String -- or ByteString
  -- FIXME[E2]: replace String, want constant time extraction!


---- The PT Monad Transformer --------------------------------------

-- PT - Parser [Monad] Transformer
type PT m a = ExceptT Errors (ReaderT Contents m) a
  -- FIXME[R2]: make abstract.

runPT :: PT m a -> Contents -> m (Possibly a)
runPT m = runReaderT (runExceptT m)


---- primitives ----------------------------------------------------

mkPrimSRP :: Monad m => Width -> (Contents -> Possibly a        ) -> SRP m a
mkPrimDRP :: Monad m => WC    -> (Contents -> Possibly (a,Width)) -> DRP m a

  -- and for now, we're ignoring non-failing parsers!  Because?
  --  - Optimal doesn't currently/easily support.
  --  - Adds confusion to the syntax.
  --  - How much have we lost?  In most any parser there will be at least
  --    some possibility for failure, so we'll be "lifting" at some point.

mkPrimSRP w f =
  (w, \r-> do
           cs <- unsafeReadRegion r
           except $ f cs
           -- note that w is checked during apply
  )

mkPrimDRP wc f =
  ( wc
  , \r-> do
         r' <- except $ subRegionMax r 0 wc
           -- get region that satisfies the 'wc' constraint
         cs <- readRegion r'
         (a,w) <- except $ f cs
         --  assert (checkWC wc w) -- ??  FIXME!!
         return (a, snd $ R.split1 r' w) -- ~obscure
         -- FIXME: TODO: return good error msg
  )


---- Applying Parsers to Regions -----------------------------------

p @!  x = p `appSRP'` x
p @@! x = p `appDRP` x

appSRP :: Monad m => SRP m a -> Region -> PT m a
appSRP (w,p) r =
  if R.r_width r == w then
    p r
  else
    throwE
      [ unwords [ "appSRP'': width mismatch. expecting"
                , show w
                , "found"
                , show r
                ]]

appDRP :: Monad m => DRP m a -> Region -> PT m ((a,Region),Region)
appDRP (wc,p) r0 =
  do
  mCheckWC wc r0
  (a,r1) <- p r0
  return ((a, r0 `R.regionMinusSuffix` r1), r1)

appDRP' :: Monad m => DRP m a -> Region -> PT m (a,Region)
appDRP' p r =
  do
  ((a,_),r') <- appDRP p r
  return (a,r')

-- Using appSRP' and appDRP can reduce many needs for explicit region
-- splitting/etc.

appSRP' :: Monad m => SRP m a -> Region -> PT m ((a,Region),Region)
appSRP' p r =
  do
  let w = srpWidth p
  (r1,r2) <- except (R.split1P r w)
  a <- p `appSRP` r1
  return ((a,r1),r2)



---- abstractions / using ------------------------------------------

-- these will correctly set appropriate widths/WCs:

pairSRPs :: SRP m a -> SRP m b -> SRP m (a,b)
pairSRPs = niy

sequenceSRPs :: [SRP m a] -> SRP m [a]
sequenceSRPs = niy

sequenceDRPs :: DRP m a -> DRP m b -> DRP m (a,b)
sequenceDRPs = niy
  -- useful when intermediate regions unimportant.

pManySRPs :: Monad m => Int -> SRP m a -> SRP m [a]
pManySRPs i p =
  assert (i >= 0) $
  ( fromIntegral i * widthSingle
  , \r-> do
         let rs = R.splitWidths r (replicate i widthSingle)
         mapM (appSRP p) rs
  )
  where
  widthSingle = srpWidth p

---- internal monadic primitives, not exported -------------------------------

-- | monadic primitive to extract a region of the file Contents
--
-- This may fail (in the monad), because the region may be out of range.
readRegion :: Monad m => Region -> PT m Contents
readRegion r = do
               s <- lift ask
               except $ extractRegion r s

-- | monadic primitive to extract a region of the file Contents
--
-- can use this if you know the region is good.
--   FIXME: this really used (now that Fail/NoFail merged, no use for).
unsafeReadRegion :: Monad m => Region -> PT m Contents
unsafeReadRegion r = do
                     s <- lift ask
                     case extractRegion r s of
                       Left e   -> error (concat e)
                       Right cs -> return cs


---- Region operations (don't need 'PT m') -------------------------
-- FIXME: move to ...?

extractRegion :: Region -> Contents -> Possibly Contents
extractRegion (R st wd) c =
  if st + wd <= clen then
    Right $ genericTake wd $ genericDrop st c
  else
    Left ["extractRegion: region extends beyond contents"]

  where
  clen = genericLength c

  -- FIXME[E1]: inefficient!

-- exposed:
subRegion :: Region -> Offset -> Width -> Possibly Region
subRegion = R.subRegionP

-- exposed:
-- | subRegionMax r o wc - extracts the largest subregion from r at offset 0
--   that satisfies the constraint 'wc':
subRegionMax :: Region -> Offset -> WC -> Possibly Region
subRegionMax (R s w) l wc =
  if l < w && checkWC wc (w-l) then
    Right (R (s+l) w')
  else
    Left ["extractRegion_DynWd_Fail: region cannot hold 'wc'"]

  where
  w' = case maxWidth wc of
         MW mw    -> min mw (w-l)
         MW_NoMax -> w-l

mCheckWC :: Monad m => WC -> Region -> PT m ()
mCheckWC wc r = if checkWC wc (r_width r) then
                  return ()
                else
                  throwE [unwords [ "mcheckWC fail:"
                                  , show wc
                                  , show r
                                  ]
                         ]

---- The LR implementation -----------------------------------------

-- NOTE: these can only be applied to 'matching' regions
-- TODO: turn the following into abstract datatypes:
--   - use short names and use OverloadedRecordDot

type SRP m a = (Width, Region -> PT m a)           -- Static Region Parser
type DRP m a = (WC   , Region -> PT m (a,Region))  -- Dynamic Region Parser

srpWidth  :: SRP m a -> Width
drpWidthC :: DRP m a -> WC
srpWidth  = fst
drpWidthC = fst

---- utilities -----------------------------------------------------

niy = error "niy"
