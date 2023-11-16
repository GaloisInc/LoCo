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

type PT m a = ExceptT Errors (ReaderT Contents m) a

runPT :: PT m a -> Contents -> m (Possibly a)
runPT m = runReaderT (runExceptT m)



---- The LR abstraction (exposed) --------------------------------

runParser :: Parser a -> Contents -> Possibly a
runParser = niy

-- instance Monad Parser where {}

type Parser a = ParserImplem a
                  -- TODO: Replace with PT m

-- Hmmm: use Parser m type-class instead?
-- class Monad m => Parser m where {}

getWidth  :: SRP a -> Width
getWC_DRP :: DRP a -> WC

mkPrimSRP :: Width -> (Contents -> Possibly a        ) -> SRP a
mkPrimDRP :: WC    -> (Contents -> Possibly (a,Width)) -> DRP a

  -- and for now, we're ignoring non-failing parsers!  Because?
  --  - Optimal doesn't currently/easily support.
  --  - Adds confusion to the syntax.
  --  - How much have we lost?  In most any parser there will be at least
  --    some possibility for failure, so we'll be "lifting" at some point.

p @!  x = p `appSRP` x
p @@! x = p `appDRP` x

appSRP :: SRP a -> Region -> Parser a
appSRP = undefined

appDRP :: DRP a -> Region -> Parser ((a,Region),Region)
appDRP = undefined

-- Using appSRP' and appDRp can reduce many needs for explicit region
-- splitting/etc.

subRegion :: Region -> Offset -> Width -> Possibly Region
subRegion = R.subRegionP
  -- aha: not in Parser, is pure.

---- maybe useful: ... ---------------------------------------------

subRegionMax :: Region -> Offset -> WC -> Possibly Region
subRegionMax = niy


---- abstractions / using ------------------------------------------

appSRP' :: SRP a -> Region -> Parser ((a,Region),Region)
appSRP' = undefined

sequenceSRPs :: SRP a -> SRP b -> SRP (a,b)
sequenceSRPs = niy

sequenceDRPs :: DRP a -> DRP b -> DRP (a,b)
sequenceDRPs = niy
  -- useful when intermediate regions unimportant.

-- both these last will correctly determine widths/WCs.


---- old implem ----------------------------------------------------

---- internal monadic primitives, not exported -------------------------------

-- | monadic primitive to extract a region of the file Contents
--
-- This may fail, because the region may be out of range.
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

extractRegion :: Region -> Contents -> Possibly Contents
extractRegion (R st wd) c =
  if st + wd <= clen then
    Right $ genericTake wd $ genericDrop st c
  else
    Left ["extractRegion: region extends beyond contents"]

  where
  clen = genericLength c

  -- FIXME[E1]: inefficient!

---- The LR implementation (hidden) ------------------------------

getWidth = niy
getWC_DRP = niy

mkPrimSRP = niy
mkPrimDRP = niy

-- the implementation of Parser:
type ParserImplem a = IO a
                      -- TODO: reader/exception

{-
Q. Any difference between these:

    mkPrimSRP w p
    mkPrimDRP (widthToWC w) (fmap (addWidth w) . p)

  - well, the TYPES are different!
-}

data RgnParser m a =
  RP { w :: WidthConstraint, p :: Region -> Parser a}

data SRP a -- = (WC, Region -> Fail m a)           -- Static Region Parser
data DRP a -- = (WC, Region -> Fail m (a,Region))  -- Dynamic Region Parser

---- exploring syntax ----------------------------------------------

test = RP undefined undefined
-- testw = test.w
testw = w test  -- FIXME


---- utilities -----------------------------------------------------

niy = error "niy"

type Byte = Int
