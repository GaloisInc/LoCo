module Language.OptimalPEAR.Examples.ICC_Demo where

-- local PEAR modules:
import           Language.OptimalPEAR.RunOptimal
import           Language.PEAR.Primitives
import           Language.PEAR.Region.API (Region)

-- local ICC modules:
import           Language.OptimalPEAR.Examples.ICC_Optimal
import           Language.OptimalPEAR.Examples.ICC_Inputs


---- run ICC -------------------------------------------------------

iccPrims :: [(String, ICC (FailT IO) -> FailT IO String)]
iccPrims =
  [ ("cnt"       , forceAndShow . cnt)
  , ("teds"      , forceAndShow . teds)
  , ("teds_safe" , forceAndShow . teds_safe)
  , ("cavityFree", forceAndShow . cavityFree)
  , ("cnt_r"     , forceAndShow . cnt_r)
  , ("r2"        , forceAndShow . r2)
  , ("rs"        , forceAndShow . rs )
  , ("tblR3"     , forceAndShow . tblR3)
  , ("tbl"       , forceAndShow . tbl)
  , ("r3"        , forceAndShow . r3)
  , ("teds_rs"   , forceAndShow . teds_rs)
  , ("canon_rs"  , forceAndShow . canon_rs)
  ]
           
run_ICC_d1 = run_ICC d1

run_ICC = run icc
              iccPrims
              ["rs","cnt"]
             
runI_ICC = runI icc iccPrims
           
---- run Optimal module with list of commands ----------------------
-- 
-- | run pmod prims inputCommands inputString - run ...
-- 
--   pmod is a parameterized module, takes a region

run :: (Region -> FailT IO (env (FailT IO)))         -- mkModule
    -> [(String, env (FailT IO) -> FailT IO String)] -- force & show
    -> [String]                                      -- sequence of prims
    -> Contents
    -> IO ()
run = run' (flip runFailT_IO)

---- run Optimal module interactively ------------------------------
                  
-- | runI mod prims inputString - run interactively
runI :: (Region -> FailT IO (env (FailT IO)))         -- mkModule
    -> [(String, env (FailT IO) -> FailT IO String)] -- force & show
    -> Contents
    -> IO ()    
runI = runI' (flip runFailT_IO)


