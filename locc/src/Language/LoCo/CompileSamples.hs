-- TH
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-splices #-}
{-# OPTIONS_GHC -dsuppress-uniques #-}

module Language.LoCo.CompileSamples (module Language.LoCo.CompileSamples, runParser) where

import Language.LoCo.Compile (declareEntrypoint, declareParser, declareType)
import Language.LoCo.Parser (Parser, Thunk, force, manyT, onSubRegion, parseU8, runParser, topRegion)
import Language.LoCo.Region (Region, rDrop, rTake)
import Language.LoCo.Samples (fifthElement, lEntry, lvParser, lvType)

declareType lvType

declareParser "parseLV" lvParser

declareEntrypoint "five" fifthElement
declareEntrypoint "getL" lEntry