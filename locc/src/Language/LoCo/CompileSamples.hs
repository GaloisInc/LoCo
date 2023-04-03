{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Language.LoCo.CompileSamples where

import Language.LoCo.Compile (declareParser, declareType)
import Language.LoCo.Parser (Parser, Thunk, manyT, onSubRegion, parseU8)
import Language.LoCo.Region (Region, rDrop, rTake)
import Language.LoCo.Samples (lvParser, lvType)

declareType lvType

declareParser "parseLV" lvParser