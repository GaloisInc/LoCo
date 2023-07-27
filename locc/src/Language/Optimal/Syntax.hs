{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Optimal.Syntax where

import Data.Map (Map)
import Data.Text (Text)
import Language.Haskell.TH (Exp)

data ModuleDecl = ModuleDecl
  { modOriginalTy :: Type, -- ^ the type as the user wrote it
    modExpandedTy :: Type, -- ^ the type with all aliases expanded
    modName :: Symbol,
    modParams :: [Symbol],
    modEnv :: Env Exp
  }
  deriving (Eq, Show)

data TypeDecl = TypeDecl
  { tdName :: Symbol,
    tdType :: Type
  }
  deriving (Show)

data Type
  = Alias Symbol
  | List Type
  | Tuple [Type]
  | Arrow Type Type
  | Rec (Env Type)
  deriving (Eq, Show)

type Symbol = Text

type Env a = Map Symbol a
