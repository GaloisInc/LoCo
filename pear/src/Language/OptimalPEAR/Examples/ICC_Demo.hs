{-# OPTIONS_GHC -Wno-unused-imports #-}
  -- FIXME!

module Language.OptimalPEAR.Examples.ICC_Demo where

-- base pkgs:
import           Control.Monad
import           Data.Word
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Identity
import           Control.Monad.Trans.Except
import           Data.Word (Word64, Word8)

-- package locc (optimal):
import           Language.Optimal.Quote (optimal)
import           Thunk.RefVal (Thunked, delayAction, force)

-- local modules:
import           Language.PEAR.Primitives
import           Language.PEAR.ParserLibrary
import           Language.PEAR.Region.API (Region,r_width)
import qualified Language.PEAR.Region.API as R
import           Language.PEAR.Types
import           Language.PEAR.Util

-- ICC
import           Language.OptimalPEAR.Examples.ICC_Optimal
import           Language.OptimalPEAR.Examples.ICC_Inputs


---- run ICC -------------------------------------------------------

iccPrims :: [(String, ICC (FailT IO) -> FailT IO String)]
iccPrims = [("rs" , forceAndShow . rs )
           ,("cnt", forceAndShow . cnt)
           ]
           
run_ICC = run (\_-> icc)
              iccPrims
              ["rs","cnt"]
             
runI_ICC = runI (\_-> icc)
                iccPrims

           
run_ICC_d1 = run_ICC d1



---- utilities -----------------------------------------------------

forceAndShow :: (MonadIO m, Show a) => Thunked m a -> m String
forceAndShow tx =
  do
  x <- force tx
  return (show x)
              

---- run Optimal module with list of commands ----------------------

-- | run mod prims inputCommands inputString - run ...
run :: (Region -> FailT IO (env (FailT IO)))         -- mkModule
    -> [(String, env (FailT IO) -> FailT IO String)] -- force & show
    -> [String]                                      -- sequence of prims
    -> Contents
    -> IO ()    
run mkModule prims syms contents =
  do
  pR <- flip runFailT_IO contents $
         do
         let globalRegion = R.R 0 (toLoc $ length contents)
         liftIO $ setTopLevelRegion globalRegion  -- FIXME: Adhoc1
         mod' <- mkModule globalRegion  -- module, instantiated
         let doCmd sym =
               case lookup sym prims of
                 Just f  -> do
                            s <- f mod'
                            liftIO $ putStrLn
                              $ unwords [sym, "evaluated to", s]
                 Nothing -> liftIO $ putStrLn
                              $ unwords ["symbol",sym,"unknown"]
         mapM_ doCmd syms
         
  case pR of
    Left ss  -> mapM_ putStrLn ("program exited with:" : ss)
    Right () -> do
                putStrLn ("program exited cleanly")


---- run Optimal module interactively ------------------------------
                  
-- | runI mod prims inputString - run interactively
runI :: (Region -> FailT IO (env (FailT IO)))         -- mkModule
    -> [(String, env (FailT IO) -> FailT IO String)] -- force & show
    -> Contents
    -> IO ()    
runI mkModule prims contents =
  do
  _ <- flip runFailT_IO contents $
    do
    let globalRegion = R.R 0 (toLoc $ length contents)
    liftIO $ setTopLevelRegion globalRegion  -- FIXME: Adhoc1
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
