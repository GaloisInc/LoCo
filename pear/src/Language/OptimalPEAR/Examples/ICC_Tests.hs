{-# LANGUAGE OverloadedRecordDot #-}

module Language.OptimalPEAR.Examples.ICC_Tests where

-- local modules:
import           Language.OptimalPEAR.Examples.ICC_Spec
import           Language.OptimalPEAR.Examples.ICC_Inputs

import           Language.PEAR.API

runTest = applyToContents icc_pear

runtestsG = mapM_ (\i -> runTest i >>= print) ds

runtestsE = mapM_ (\i -> runTest i >>= print) es
