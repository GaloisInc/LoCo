{-# LANGUAGE LambdaCase #-}

module Language.PEAR.Primitives where  

-- base pkgs:
import Control.Exception (assert)
import Control.Monad
import Data.List
-- import Debug.Trace

-- transformer pkg:
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except
import Data.Functor.Identity

-- local modules:
import Language.PEAR.Region.API
import Language.PEAR.Types
import Language.PEAR.Util


-- NOTE: FailT and NoFlT are brothers,
--   - we plan to apply them to the same base monads
--   - but note that FailT is built from NoFlT (is two monad transformers!)

newtype FailT m a = FailT {runFailT :: ExceptT Errors (NoFlT m) a}
                    deriving (Applicative,Functor,Monad,MonadIO)

newtype NoFlT m a = NoFlT {runNoFlT :: ReaderT Contents m a}
                    deriving (Applicative,Functor,Monad,MonadIO)

type ExceptT' m a = ExceptT Errors m a
  -- Useful?  FIXME.

type Contents = String
  -- FIXME[E2]: replace String, want constant time extraction!


---- explore -------------------------------------------------------

type Env a = [(Symbol,a)] -- for now
type PT m a = ExceptT Errors (ReaderT Contents m) a
type Symbol = String

type Pair a = (a,a)

{-
int0 :: (Show a, MonadTrans t)
     => Pair (a -> t m a)
     -> a -> IO (t m a)
-}

-- essence of interpret: sequencing 'm' and inserting IO.
interp0 :: (MonadIO m, Show b) => (a -> m b, b -> m c) -> a -> m c
interp0 (f,g) a = do
               x1 <- f a
               liftIO $ print x1
               g x1

test1 :: Int -> IO (Possibly Int)
-- test1 :: MonadIO m => Int -> m (Possibly Int)
test1 x = runPT (interp0 (f,g) x) "unused"
  where
  f :: Monad m => Int -> PT m Int
  f i = return (i+1)
  
  g :: Monad m => Int -> PT m Int
  g i = return (i*2)

-- runPT :: PT m a -> Contents -> m a
runPT :: PT m a -> Contents -> m (Possibly a)
runPT m = runReaderT (runExceptT m)


interpret :: MonadIO m => Env ([a] -> m a) -> Env (m a)
interpret = stub


{-
---- New (FIXME: integrate) --------------------------------------------------

runNoFlT_IO :: NoFlT IO a -> Contents -> IO a
runNoFlT_IO = runNoFlT'

runNoFlT' :: NoFlT m a -> Contents -> m a
runNoFlT' m s = runReaderT (runNoFlT m) s

runFailT' :: FailT m a -> Contents -> m (Possibly a)
runFailT' (FailT m) s = runNoFlT' (runExceptT m) s

  -- DIFF between this and 10lines below
  
type FailIO a = FailT IO a
type NoFlIO a = NoFlT IO a
  
-- FIXME: update ^ to create region from contents!

-- FIXME: ensure we need these, rewrite module using these!



---- Two Base Monads ---------------------------------------------------------

-- FIXME: deprecated!!
type FailM = ExceptT Errors NoFlM       -- Can fail
type NoFlM = ReaderT Contents Identity  -- Cannot Fail

  -- This reader monad
  --  - allows access to the contents without having to thread it through.
  --  - might in future
  --    - do sanity checks on the regions extracted

-- 'primitive' run functions:

runFailM :: FailM a -> Contents -> Possibly a
runFailM (ExceptT m) c = runNoFlM m c

runNoFlM :: NoFlM a -> Contents -> a
runNoFlM m c = runReader m c

failM :: Errors -> FailM a
failM = throwE

---- running RgnPrsrs (NEW, T based) -----------------------------------------

run_RgnPrsr_FxdWd_NoFlT
  :: Monad m => RgnPrsr_FxdWd_NoFlT m a -> Contents -> m a
run_RgnPrsr_FxdWd_NoFlT fwp cs =
  runNoFlT' (app_FxdWd_NoFlT fwp globalRegion) cs
  where
  globalRegion = R 0 (toLoc $ length cs)

  
---- running RgnPrsrs --------------------------------------------------------
-- user accessible run functions:

run_RgnPrsr_FxdWd_Fail :: RgnPrsr_FxdWd_Fail a -> Contents -> Possibly a
run_RgnPrsr_FxdWd_Fail fwp cs =
  runFailM (app_FxdWd_Fail fwp globalRegion) cs
  where
  globalRegion = R 0 (toLoc $ length cs)

run_RgnPrsr_FxdWd_NoFl :: RgnPrsr_FxdWd_NoFl a -> Contents -> a
run_RgnPrsr_FxdWd_NoFl fwp cs =
  runNoFlM (app_FxdWd_NoFl fwp globalRegion) cs
  where
  globalRegion = R 0 (toLoc $ length cs)

-- FIXME: this useful?
runWithWidth_RgnPrsr_FxdWd_Fail :: (Width -> RgnPrsr_FxdWd_Fail a) -> Contents -> Possibly a
runWithWidth_RgnPrsr_FxdWd_Fail fwp cs =
  runFailM (app_FxdWd_Fail (fwp w) globalRegion) cs
  where
  w = genericLength cs
  globalRegion = R 0 (toLoc w)

  

run_RgnPrsr_DynWd_Fail :: RgnPrsr_DynWd_Fail a -> Contents -> Possibly a
run_RgnPrsr_DynWd_Fail p cs =
  fst <$> run_RgnPrsr_DynWd_Fail' p cs

run_RgnPrsr_DynWd_Fail' :: RgnPrsr_DynWd_Fail a -> Contents -> Possibly (a, Region)
run_RgnPrsr_DynWd_Fail' p cs =
  runFailM (app_DynWd_Fail p globalRegion) cs
  where
  globalRegion = R 0 (toLoc $ length cs)


---- Region primitives, exportable -------------------------------------------

-- FIXME[F2]: neither of these used!

extractRegion_FxdWd_Fail :: Region -> Loc -> Width -> FailM Region
extractRegion_FxdWd_Fail (R s w) l w' =
  do
  when (l + w' > w) $
    throwE [unwords
             [ "extractRegion_FxdWd_Fail:"
             , "cannot extract", show l, show w'
             , "from region", show (R s w)
             ]]
  return (R (s+l) w')
                               
extractRegion_DynWd_Fail :: Region -> Loc -> WidthConstraint -> FailM Region
extractRegion_DynWd_Fail (R s w) l wc =
  do
  unless (l < w && checkWC wc (w-l)) $
    throwE ["extractRegion_DynWd_Fail: region cannot hold 'wc'"]
  return (R (s+l) w')
                               
  where
  w' = case maxWidth wc of
         MW mw    -> min mw (w-l)
         MW_NoMax -> w-l


---- internal monadic primitives, not exported -------------------------------

-- | monadic primitive to extract a region of the file Contents
--
-- This may fail, because the region may be out of range.
readRegion :: Region -> FailM Contents
readRegion r = do
               s <- lift ask
               case extractRegion r s of
                 Left e   -> throwE e
                 Right cs -> return cs

-- | monadic primitive to extract a region of the file Contents
--
-- can use this if you know the region is good.
unsafeReadRegion :: Region -> NoFlM Contents
unsafeReadRegion r = do
                     s <- ask
                     case extractRegion r s of
                       Left e  -> error (concat e)
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

---- FIXME: NEW/delete/huh? --------------------------------------------------
{-
run_FxdWd_Fail = execFxdWd_Fail runFailM

-- run_RgnPrsr_FxdWd_Fail' :: RgnPrsr_FxdWd_Fail m a -> Contents -> Possibly a
execFxdWd_Fail run fwp cs =
  run (app_FxdWd_Fail' fwp globalRegion) cs
  where
  globalRegion = R 0 (toLoc $ length cs)

type RgnPrsr_FxdWd_Fail' m a = (Width, Region -> m a)
type RgnPrsr_DynWd_Fail' m a = (WidthConstraint, Region -> m (a, Region))

app_FxdWd_Fail' :: Monad m => RgnPrsr_FxdWd_Fail' m a -> Region -> m a
app_FxdWd_Fail' (w,p) r =
  if r_width r == w then
    p r
  else
    error $
    unwords [ "app_FxdWd_Fail: width mismatch. expecting"
            , show w
            , "found"
            , show r
            ]
-}


---- Region Parser (RgnPrsr) Varieties: 2x2 ----------------------------------

-- OLD:
type RgnPrsr_FxdWd_NoFl a = (Width, Region -> NoFlM a)
type RgnPrsr_FxdWd_Fail a = (Width, Region -> FailM a)

type RgnPrsr_DynWd_NoFl a = (WidthConstraint, Region -> NoFlM (a, Region))
type RgnPrsr_DynWd_Fail a = (WidthConstraint, Region -> FailM (a, Region))

-- NEW:
type RgnPrsr_FxdWd_NoFlT m a = (Width, Region -> NoFlT m a)
type RgnPrsr_FxdWd_FailT m a = (Width, Region -> FailT m a)

type RgnPrsr_DynWd_NoFlT m a = (WidthConstraint, Region -> NoFlT m (a, Region))
type RgnPrsr_DynWd_FailT m a = (WidthConstraint, Region -> FailT m (a, Region))

  -- Note that if parser fails, we will not capture any "remaining region".

-- FIXME[R2]: turn ^ pairs into constructors!, thus get below:

width_FxdWd :: (Width, Region -> a) -> Width
width_FxdWd (w,_) = w

widthc_DynWd_Fail :: (WidthConstraint, Region -> a) -> WidthConstraint
widthc_DynWd_Fail = fst


---- RgnPrsr, Etc. Abstractions ----------------------------------------------

check_M :: Possibly b -> FailM b
check_M = \case
             Left s  -> throwE s
             Right x -> pure x

check_M' :: String -> Possibly b -> FailM b
check_M' s = \case
                Left ss -> throwE (s:ss)
                Right x -> pure x
             
split1_M :: Region -> Width -> FailM (Region, Region)
split1_M r w = check_M $ split1_Possibly r w 

subRegion_M :: Region -> Loc -> Width -> FailM Region
subRegion_M r s w = check_M $ subRegion_Possibly r s w


lift_NoFlM :: NoFlM a -> FailM a
lift_NoFlM = lift
  
lift_FxdWd_NoFl :: RgnPrsr_FxdWd_NoFl a -> RgnPrsr_FxdWd_Fail a
lift_FxdWd_NoFl (w,p) = (w, \r-> lift_NoFlM (p r))

lift_NoFlT :: Monad m => NoFlT m a -> FailT m a
lift_NoFlT m  = FailT (lift m)
  
lift_FxdWd_NoFlT :: Monad m => RgnPrsr_FxdWd_NoFlT m a -> RgnPrsr_FxdWd_FailT m a
lift_FxdWd_NoFlT (w,p) = (w, \r-> lift_NoFlT (p r))


---- primitives to create Region Parsers -------------------------------------

mkPrim_FxdWd_NoFl :: Width -> (String -> a) -> RgnPrsr_FxdWd_NoFl a
mkPrim_FxdWd_NoFl w f =
  (w, \r-> do
           cs <- unsafeReadRegion r
           return $ f cs
           -- note that w is checked during apply
  )

-- | create RgnPrsr_DynWd_Fail, the primitive f returns how many bytes parsed
mkPrim_DynWd_Fail :: WidthConstraint -> (String -> Possibly (a,Width)) -> RgnPrsr_DynWd_Fail a
mkPrim_DynWd_Fail wc f =
  ( wc
  , \r-> do
         r' <- extractRegion_DynWd_Fail r 0 wc  -- constrain region
         cs <- readRegion r'
         case f cs of
           Left e      -> throwE e
           Right (a,w) -> assert (checkWC wc w)      
                        $ return (a, snd $ split1 r' w) -- ~obscure
                        -- TODO: return good error msg
  )


---- NEW: Transformer based: RgnPrsr: app_*'s --------------------------------

app_FxdWd_NoFlT  :: Monad m =>
                    RgnPrsr_FxdWd_NoFlT m a -> Region -> NoFlT m a
app_FxdWd_NoFlT (w,p) r =
  if r_width r == w then
    p r
  else
    error $
    unwords [ "app_FxdWd_NoFl: width mismatch. expecting"
            , show w
            , "found"
            , show r
            ]

app_FxdWd_FailT  :: Monad m =>
                    RgnPrsr_FxdWd_FailT m a -> Region -> FailT m a
app_FxdWd_FailT (w,p) r =
  if r_width r == w then
    p r
  else
    error $
    unwords [ "app_FxdWd_Fail: width mismatch. expecting"
            , show w
            , "found"
            , show r
            ]

mkPrim_FxdWd_NoFlT :: Monad m =>
  Width -> (String -> a) -> RgnPrsr_FxdWd_NoFlT m a
mkPrim_FxdWd_NoFlT w f =
  (w, \r-> do
           cs <- unsafeReadRegionT r
           return $ f cs
           -- note that w is checked during apply
  )


-- can use this if you know the region is good.
unsafeReadRegionT :: Monad m => Region -> NoFlT m Contents
unsafeReadRegionT r = NoFlT $ do
                      s <- ask
                      case extractRegion r s of
                        Left e  -> error (concat e)
                        Right cs -> return cs
                       

---- RgnPrsr: app_*'s --------------------------------------------------------

app_FxdWd_Fail  :: RgnPrsr_FxdWd_Fail a -> Region -> FailM a
app_FxdWd_NoFl  :: RgnPrsr_FxdWd_NoFl a -> Region -> NoFlM a
app_DynWd_Fail  :: RgnPrsr_DynWd_Fail a -> Region -> FailM (a, Region)
app_DynWd_Fail' :: RgnPrsr_DynWd_Fail a -> Region -> FailM ((a, Region), Region)

  -- | app_DynWd_Fail' - when we want to know the region parsed as well as the
  -- region remaining.  Could also get this by Region subtraction.
  
app_FxdWd_Fail (w,p) r =
  if r_width r == w then
    p r
  else
    error $
    unwords [ "app_FxdWd_Fail: width mismatch. expecting"
            , show w
            , "found"
            , show r
            ]

app_FxdWd_NoFl (w,p) r =
  if r_width r == w then
    p r
  else
    error $
    unwords [ "app_FxdWd_NoFl: width mismatch. expecting"
            , show w
            , "found"
            , show r
            ]

app_DynWd_Fail (wc,p) r = assert (checkWC wc (r_width r))
                        $ p r

app_DynWd_Fail' (wc,p) r = assert (checkWC wc (r_width r))
                          $ do
                            (a,r2) <- p r
                            return ((a, r `regionMinusSuffix` r2), r2)
                           
  
-}
