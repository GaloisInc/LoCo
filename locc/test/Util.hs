{-# LANGUAGE TemplateHaskellQuotes #-}

module Util where

import Language.Haskell.TH.Syntax (Exp, Q, loc_module, qLocation)

moduleName :: Q Exp
moduleName =
  do
    modName <- loc_module <$> qLocation
    [|modName|]
