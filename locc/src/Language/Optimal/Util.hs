{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Language.Optimal.Util where

import Data.String (IsString (..))
import Data.Text (Text)
import Data.Text qualified as Text
import Language.Haskell.TH (Name, mkName)

class Named a where
  name :: a -> Name

instance Named String where
  name = mkName

instance Named Text where
  name = mkName . Text.unpack

instance Named Name where
  name = id

instance IsString Name where
  fromString = mkName
