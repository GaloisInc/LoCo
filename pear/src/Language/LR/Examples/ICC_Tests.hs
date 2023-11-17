{-# LANGUAGE OverloadedRecordDot #-}

module Language.LR.Examples.ICC_Tests where

-- local modules:
import           Language.LR.Examples.ICC_Spec
import           Language.OptimalPEAR.Examples.ICC_Inputs

import           Language.LR.API
-- import           Language.PEAR.Types
-- import           Language.PEAR.Util

-- runTest :: Monad m => Contents -> m (Possibly [TED])
runTest s = runPT s icc_pear

runtestsG = mapM_ (\i -> runTest i >>= print) ds

runtestsE = mapM_ (\i -> runTest i >>= print) es
