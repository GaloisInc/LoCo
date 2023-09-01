module Language.Optimal.Typecheck where

import Data.Map qualified as Map
import Language.Optimal.Syntax (Env, Type (..))

-- | Recursively elaborate all aliases in a type according to the provided
-- declaration map
expandType :: Env Type -> Type -> Type
expandType tyEnv ty =
  case ty of
    Alias alias -> maybe ty go (tyEnv Map.!? alias)
    Arrow t1 t2 -> Arrow t1 (go t2)
    Tuple ts -> Tuple (map go ts)
    List t -> List (go t)
    Rec s env -> Rec s (go <$> env)
  where
    go = expandType tyEnv
