module Language.OptimalPEAR.RunOptimal where

-- base pkgs:
import           Control.Monad.IO.Class
import           Data.Vector qualified as V

-- package optimal:
import           Thunk.RefVal (Thunked, force)
import           Thunk.Vector

-- local PEAR modules:
import           Language.PEAR.Region.API (Region)
import qualified Language.PEAR.Region.API as R
import           Language.PEAR.Util

---- utilities -----------------------------------------------------

forceAndShow :: (MonadIO m, Show a) => Thunked m a -> m String
forceAndShow tx =
  do
  x <- force tx
  return (show x)
              
forceAndShowVec :: (MonadIO m, Show a) => Thunked m (Vector m a) -> m String
forceAndShowVec tvec =
  do
  tvec' <- force tvec
  xs <- V.mapM force (vecContent tvec')
  return (show xs)
              
indexAndShow :: (MonadIO m, Show a)
             => Thunked m (Vector m a) -> Int -> m String
indexAndShow tvec i =
  do
  tvec' <- force tvec
  a <- vIndex tvec' i
  return (show a)
              

---- run Optimal module with list of commands ----------------------
-- 

-- | run' runM pmod prims inputCommands inputString - run ...
-- 
--   pmod is a parameterized module, takes a region
run' :: (MonadIO m, Eq s, Show s)
     => ([a] -> m () -> IO (Possibly ()))  -- run 'm' on contents.
     -> (Region -> m b)                    -- Region parameterized 'module'
     -> [(s, b -> m String)]               -- your force&show environment
     -> [s]                                -- list of syms to demand, the prog
     -> [a]                                -- contents to 'read'
     -> IO ()
run' runOn mkModule prims syms contents =
  do
  pR <- runOn contents $
         do
         let globalRegion = R.R 0 (toLoc $ length contents)
         mod' <- mkModule globalRegion  -- module, instantiated
         let doCmd sym =
               case lookup sym prims of
                 Just f  -> do
                            s <- f mod'
                            liftIO $ putStrLn
                              $ unwords [show sym, "evaluated to", s]
                 Nothing -> liftIO $ putStrLn
                              $ unwords ["symbol",show sym,"unknown"]
         mapM_ doCmd syms
         
  case pR of
    Left ss  -> mapM_ putStrLn ("program exited with:" : ss)
    Right () -> putStrLn ("program exited cleanly")


---- run Optimal module interactively ------------------------------
                  
-- (this more polymorphic than written; useful?)
runI' :: (MonadIO m)
      => ([a] -> m () -> IO a2)     -- run 'm' on contents
      -> (Region -> m b)            -- the Region parameterized 'module'
      -> [(String, b -> m String)]  -- force & show environment
      -> [a]                        -- contents to read
      -> IO ()
runI' runOn mkModule prims contents =
  do
  _ <- runOn contents $
    do
    let globalRegion = R.R 0 (toLoc $ length contents)
    mod' <- mkModule globalRegion  -- module, instantiated
    let go =
          do
          liftIO $ putStr "\nsymbol to get: " -- >> 'flush' (unneeded)
          sym <- liftIO $ getLine
          liftIO $ putStrLn ""
          case lookup sym prims of
            Nothing ->
              do
              liftIO $ putStrLn $ unwords [sym, "not found"]
              liftIO $ putStrLn $ " allowable: " ++ show (map fst prims)
            Just f  ->
              do
              liftIO $ putStrLn $ unwords [ "========== GET"
                                          , sym
                                          , "=========="
                                          ]
              s <- f mod'
              liftIO $ putStrLn $ unwords [sym, "is", s]
              liftIO $ putStrLn "\n\n"
          go
    go
  return ()
