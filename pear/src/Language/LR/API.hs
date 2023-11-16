{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE OverloadedRecordDot #-}

module Language.LR.API where

-- base pkgs:
import Data.List

-- transformer pkg:
-- import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
-- import Data.Functor.Identity

-- FIXME: improve these to better fit the use here
import           Language.PEAR.Types
import           Language.PEAR.Util
import qualified Language.PEAR.Region.API as R -- region
import           Language.PEAR.Region.API(Region(..))

---- Types ---------------------------------------------------------

type Offset = Loc
  -- Loc - connotes absolute file location
  -- Offset - connotes relative file/region byte offset

type WC = WidthConstraint

type Contents = String -- or ByteString
  -- FIXME[E2]: replace String, want constant time extraction!


---- The PT Monad Transformer --------------------------------------

-- PT - Parser [monad] Transformer
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
           case f cs of
             Left e  -> throwE e
             Right a -> return a
           -- note that w is checked during apply
  )

mkPrimDRP wc f =
  ( wc
  , \r-> case subRegionMax r 0 wc of
           Left e   -> throwE e
           Right r' ->
               -- get region that fits in 'wc'
               do
               cs <- readRegion r'
               case f cs of
                 Left e      -> throwE e
                 Right (a,w) ->
                   --  assert (checkWC wc w) -- ??  FIXME!!
                   return (a, snd $ R.split1 r' w) -- ~obscure
                   -- FIXME: TODO: return good error msg
  )


---- applications --------------------------------------------------

p @!  x = p `appSRP` x
p @@! x = p `appDRP` x

appSRP :: SRP m a -> Region -> PT m a
appSRP = undefined

appDRP :: DRP m a -> Region -> PT m ((a,Region),Region)
appDRP = undefined

-- Using appSRP' and appDRp can reduce many needs for explicit region
-- splitting/etc.

appSRP' :: SRP m a -> Region -> PT m ((a,Region),Region)
appSRP' = undefined

---- abstractions / using ------------------------------------------

sequenceSRPs :: SRP m a -> SRP m b -> SRP m (a,b)
sequenceSRPs = niy

sequenceDRPs :: DRP m a -> DRP m b -> DRP m (a,b)
sequenceDRPs = niy
  -- useful when intermediate regions unimportant.

-- both these last will correctly determine widths/WCs.


---- internal monadic primitives, not exported -------------------------------

-- | monadic primitive to extract a region of the file Contents
--
-- This may fail (in the monad), because the region may be out of range.
readRegion :: Monad m => Region -> PT m Contents
readRegion r = do
               s <- lift ask
               case extractRegion r s of
                 Left e   -> throwE e
                 Right cs -> return cs

-- | monadic primitive to extract a region of the file Contents
--
-- can use this if you know the region is good.
unsafeReadRegion :: Monad m => Region -> PT m Contents
unsafeReadRegion r = do
                     s <- lift ask
                     case extractRegion r s of
                       Left e   -> error (concat e)
                       Right cs -> return cs

---- Region operations (don't need 'PT m') -------------------------

extractRegion :: Region -> Contents -> Possibly Contents
extractRegion (R st wd) c =
  if st + wd <= clen then
    Right $ genericTake wd $ genericDrop st c
  else
    Left ["extractRegion: region extends beyond contents"]

  where
  clen = genericLength c

  -- FIXME[E1]: inefficient!

-- FIXME: move?

subRegion :: Region -> Offset -> Width -> Possibly Region
subRegion = R.subRegionP
  -- aha: not in Parser, is pure.

---- maybe useful: ... ---------------------------------------------

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

---- The LR implementation (hidden) ------------------------------


-- TODO: turn the following into abstract datatypes:
--   these can only be applied to 'matching' regions
type SRP m a = (Width, Region -> PT m a)           -- Static Region Parser
type DRP m a = (WC   , Region -> PT m (a,Region))  -- Dynamic Region Parser

srpWidth  :: SRP m a -> Width
drpWidthC :: DRP m a -> WC
srpWidth  = fst
drpWidthC = fst



---- Applications:

appSRP'' :: Monad m => SRP m a -> Region -> PT m a
appSRP'' (w,p) r =
  if R.r_width r == w then
    p r
  else
    error $  -- FIXME: into fail.
    unwords [ "appSRP': width mismatch. expecting"
            , show w
            , "found"
            , show r
            ]

---- utilities -----------------------------------------------------

niy = error "niy"
