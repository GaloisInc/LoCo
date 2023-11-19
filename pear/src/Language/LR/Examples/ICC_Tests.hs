{-# LANGUAGE OverloadedRecordDot #-}

module Language.LR.Examples.ICC_Tests where

-- local modules:
import           Language.LR.Examples.ICC_Spec
import           Language.OptimalPEAR.Examples.ICC_Inputs

import           Language.LR.API

runTest = applyToContents icc_pear

runtestsG = mapM_ (\i -> runTest i >>= print) ds

runtestsE = mapM_ (\i -> runTest i >>= print) es
