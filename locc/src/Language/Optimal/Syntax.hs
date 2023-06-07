{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Optimal.Syntax where

import Data.Map (Map)
import Data.Text (Text)
import Language.Haskell.TH (Exp)

data NamedEnv e = NamedEnv {neName :: Symbol, neEnv :: Env e}
  deriving (Show)

data OptimalModule = OptimalModule {pmTy :: Symbol, pmEnv :: NamedEnv Exp}
  deriving (Show)

newtype OptimalTypeDecl = OptimalTypeDecl (NamedEnv OptimalType)
  deriving (Show)

data OptimalType
  = Bool
  | Char
  | Ctor Symbol
  deriving (Show)

type Symbol = Text

type Env a = Map Symbol a
