{-# LANGUAGE TypeSynonymInstances #-}
-- {-# LANGUAGE OverloadedRecordDot #-}

module PEARL where

-- import Language.PEAR.Types

---- Types ---------------------------------------------------------

type Width  = Int
type Loc    = Int -- ??
type Offset = Int -- new, better name

data WidthConstraint
type WC = WidthConstraint

type Contents = String -- or ByteString
data Region

---- The EARL abstraction (exposed) --------------------------------

runParser :: Parser a -> Contents -> Possibly a
runParser = niy

-- instance Monad Parser where {}

type Parser a = ParserImplem a
                  -- TODO: reader/exception

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

subRegion    :: Region -> Offset -> Width -> Parser Region
subRegion = undefined

-- maybe useful: ?

subRegionMax :: Region -> Offset -> WC -> Parser Region   -- ??
subRegionMax = undefined


---- abstractions / using ------------------------------------------

appSRP' :: SRP a -> Region -> Parser ((a,Region),Region)
appSRP' = undefined

sequenceSRPs :: SRP a -> SRP b -> SRP (a,b)
sequenceSRPs = niy

sequenceDRPs :: DRP a -> DRP b -> DRP (a,b)
sequenceDRPs = niy
  -- useful when intermediate regions unimportant.

-- both these last will correctly determine widths/WCs.

---- The EARL implementation (hidden) ------------------------------

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

type Possibly a = Either [String] a

niy = error "niy"

data TBD
type Byte = Int
