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


---- run ICC -------------------------------------------------------

icc2Prims :: [(String, ICC (FailT IO) -> FailT IO ())]
icc2Prims = [("rs" , forceAndPrint . rs )
            ,("cnt", forceAndPrint . cnt)
            ]
           
runICC = run (\_-> icc2)
             icc2Prims
             ["rs"]
             "0000"  -- bogus
                     
forceAndPrint :: (MonadIO m, Show a) => Thunked m a -> m ()
forceAndPrint tx =
  do
  x <- force tx
  liftIO $ print x
              
---- run Optimal module --------------------------------------------

-- NOTE: faking parameterized modules currently! (FIXME)

run :: (Region -> FailT IO (env (FailT IO)))     -- mkModule
    -> [(String, env (FailT IO) -> FailT IO ())] -- force & print primitives
    -> [String]                                  -- sequence of prims
    -> Contents
    -> IO ()    
run _        _     []      _contents = error "run"
run mkModule prims (sym:_) contents =
  do
  pR <- flip runFailT_IO contents $
         do
         let globalRegion = R.R 0 (toLoc $ length contents)
         mod' <- mkModule globalRegion  -- module, instantiated
         case lookup sym prims of
           Just f  -> f mod'
           Nothing -> liftIO $ putStrLn $ unwords ["symbol",sym,"unknown"]
         return ()
         
  case pR of
    Left ss  -> mapM_ putStrLn ("program exited with:" : ss)
    Right () -> do
                putStrLn ("program exited cleanly")

                  

