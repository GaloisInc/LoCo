{-# LANGUAGE OverloadedRecordDot #-}

module Language.LR.API
  (
  -- types:
    PT      -- FIXME: make abstract!
  , SRP
  , DRP
  , VR(..)
  , Contents

  -- what *is* exposed from SRP/DRP parsers:
  , srpWidth
  , drpWidthC

  -- runnning the PT monad transformer:
  , runPT
  , runPT'

  -- some monad operators (exposing the exception monad)
  , except
  , throwE

  -- creating parsing primitives:
  , mkPrimSRP
  , mkPrimDRP

  -- applying primitives to regions:
  , (@$)
  , (@!)
  , (@!-)
  , (@@!)
  , appDRP
  , appDRP'
  , appSRP
  , appSRP'

  -- parsing combinators:
  , pairSRPs
  , sequenceSRPs
  , pairDRPs
  , sequenceDRPs
  , pManySRPs

  -- more Region operators (beyond Language.PEAR.Region.API):
  , subRegion
  , subRegionMax

  )
where

-- base pkgs:
import           Data.List
import           Control.Exception (assert)

-- transformer pkg:
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Except

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

runPT' :: PT m a -> Contents -> m (Possibly a)
runPT' m = runReaderT (runExceptT m)

runPT :: Monad m => Contents -> (Region -> PT m a) -> m (Possibly a)
runPT contents rp = runPT' (rp globalRegion) contents
                    where
                    globalRegion = R 0 (toLoc $ length contents)


---- primitives ----------------------------------------------------

mkPrimSRP :: Monad m => Width -> (Contents -> Possibly a        ) -> SRP m a
mkPrimDRP :: Monad m => WC    -> (Contents -> Possibly (a,Width)) -> DRP m a

  -- and for now, we're ignoring non-failing parsers!  Because?
  --  - Optimal doesn't currently/easily support.
  --  - Adds confusion to the syntax.
  --  - How much have we lost?  In most any parser there will be at least
  --    some possibility for failure, so we'll be "lifting" at some point.

mkPrimSRP w f =
  SRP{ srpW= w
     , srpP= \r->
             do
             cs <- unsafeReadRegion r
             except $ elaboratePossibly ["at region " ++ R.ppRegion r] $ f cs
             -- note that w is checked during apply
     }

mkPrimDRP wc f =
  DRP{ drpC = wc
     , drpP = \r->
              do
              r' <- except $ subRegionMax r 0 wc
                -- get region that satisfies the 'wc' constraint
              cs <- readRegion r'
              (a,w) <- except
                     $ elaboratePossibly ["at region " ++ R.ppRegion r']
                     $ f cs
              --  assert (checkWC wc w) -- ??  FIXME!!
              return (a, snd $ R.split1 r' w) -- ~obscure
              -- FIXME: TODO: return good error msg
     }


---- Applying Parsers to Regions -----------------------------------

-- Using appSRP' and appDRP can reduce many needs for explicit region
-- splitting/etc.

-- naming conventions: [work on this!]
--  - @ for SRP apply
--  - @@ for DRP apply
--  - maybe:
--     _$ - throwing away region, more like $
--     _! - giving back region,

p @$  r = appSRP  p r         -- ^ parse whole region, exactly
p @!  r = appSRP' p r         -- ^ parse and return remaining region
p @!- r = fst <$> appSRP' p r -- ^ parse and drop remaining region
p @@! r = appDRP  p r         -- ^ parse-dynamically, return remaining region


appSRP :: Monad m => SRP m a -> Region -> PT m (VR a)
appSRP (SRP w p) r =
  if R.r_width r == w then
    (flip VR r) <$> p r
  else
    throwE
      [ unwords [ "appSRP: width mismatch. expecting"
                , show w
                , "found"
                , show r
                ]
      ]

appSRP' :: Monad m => SRP m a -> Region -> PT m (VR a, Region)
appSRP' p r =
  do
  let w = srpWidth p
  (r1,r2) <- except (R.split1P r w)
  a <- p `appSRP` r1
  return (a, r2)

appDRP :: Monad m => DRP m a -> Region -> PT m (VR a,Region)
appDRP (DRP wc p) r0 =
  do
  mCheckWC wc r0
  (a,r1) <- p r0
  return (VR a (r0 `R.regionMinusSuffix` r1), r1)

appDRP' :: Monad m => DRP m a -> Region -> PT m (a,Region)
appDRP' p r =
  do
  (VR a _,r') <- appDRP p r
  return (a,r')

-- | VR - Value Region pair
data VR a = VR {v :: a, r :: Region}
            deriving (Eq,Ord,Read,Show)


---- abstractions / using ------------------------------------------

pairSRPs :: Monad m => SRP m a -> SRP m b -> SRP m (a,b)
pairSRPs pa pb =
  SRP{ srpW= w'
     , srpP= \rc ->
             case R.split1P rc (srpWidth pa) of
               Left ss       -> error (unlines ss)
                                -- should never fail.
               Right (ra,rb) ->
                   do
                   a <- (srpP pa) ra
                   b <- (srpP pb) rb
                   return (a,b)
     }
  where
  w' = srpWidth pa + srpWidth pb

sequenceSRPs :: [SRP m a] -> SRP m [a]
sequenceSRPs = niy nilSRP
{-
  FIXME: _
  foldr (\srpHd srpTl -> (\a as-> a:as) <$> pairSRPs srpHd srpTl
        )
        nilSRP
  TODO: when SRP is functor/etc, becomes trivial.
-}
  where
  nilSRP :: SRP m [a]
  nilSRP = niy

pairDRPs :: DRP m a -> DRP m b -> DRP m (a,b)
pairDRPs = niy
  -- useful when intermediate regions unimportant.

sequenceDRPs :: [DRP m a] ->DRP m [a]
sequenceDRPs = niy
  -- useful when intermediate regions unimportant.

pManySRPs :: Monad m => Int -> SRP m a -> SRP m [a]
pManySRPs i p =
  assert (i >= 0) $
  SRP { srpW= fromIntegral i * widthSingle
      , srpP= \r-> do
              let rs = R.splitWidths r (replicate i widthSingle)
              mapM (fmap v . appSRP p) rs
      }
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

-- FIXME: better name?  r hasSpace w or ...
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

-- NOTE: at application, we ensure these are only applied to
-- 'conforming' regions

-- FIXME: improve SRP/DRP
--   - functor/etc instances
--   - hide 2nd argument in exports


-- | SRP - Static Region Parser
data SRP m a = SRP{ srpW :: Width
                  , srpP :: Region -> PT m a
                  }

-- | DRP - Dynamic Region Parser
data DRP m a = DRP{ drpC :: WC
                  , drpP :: Region -> PT m (a,Region)
                  }

{-
instance Functor (SRP m) where
  fmap f (w, rp) = (w, fmap f rp)
-}

srpWidth  = srpW
drpWidthC = drpC

---- utilities -----------------------------------------------------

niy = error "niy"
