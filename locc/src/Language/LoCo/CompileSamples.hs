-- TH

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}

module Language.LoCo.CompileSamples where

import Language.LoCo.Compile (declareEntrypoint, declareParser, declareType)
import Language.LoCo.Parser (Parser, Thunk, force, manyT, onSubRegion, parseU8, topRegion)
import Language.LoCo.Region (Region, rDrop, rTake)
import Language.LoCo.Samples (fifthElement, lvParser, lvType)

declareType lvType

declareParser "parseLV" lvParser

declareEntrypoint "fifthElement" fifthElement