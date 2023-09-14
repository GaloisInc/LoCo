{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Optimal.Syntax where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Language.Haskell.TH (Exp, Name)
import Language.LoCo.Toposort (topoSortPossibly)
import Language.Optimal.Compile.Haskell.Free (Free (..))
import Language.Optimal.Util

data ModuleDecl = ModuleDecl
  { modTy :: Type,
    modName :: Symbol,
    modParams :: [Symbol],
    modEnv :: Env (ModuleBinding Exp)
  }
  deriving (Eq, Show)

data ModuleBinding e
  = Expression e
  | VectorReplicate Symbol e -- vector introduction
  | VectorMap Symbol e -- vector transformation
  | VectorIndex Symbol Symbol -- vector elimination
  | ModuleIntro
      Symbol
      -- ^ module constructor fn (*not* the module type)
      [Symbol]
      -- ^ arguments
  | ModuleIndex
      Symbol
      -- ^ module name
      Symbol
      -- ^ field name
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
  | Rec Symbol (Env Type)
  deriving (Eq, Show)

type Symbol = Text

type Env a = Map Symbol a

--------------------------------------------------------------------------------

sortModuleBindings ::
  (Free e, MonadFail m, Named n) =>
  Map n (ModuleBinding e) ->
  m [(Name, ModuleBinding e)]
sortModuleBindings modEnv =
  do
    let dependencies =
          [ (var, deps)
            | (var, binding) <- Map.toList modEnv',
              let deps = Set.toList (bindingThunks modNames binding)
          ]
    orderedNames <- reverse <$> topoSortPossibly dependencies
    pure [(n, modEnv' Map.! n) | n <- orderedNames]
  where
    modEnv' = Map.mapKeys name modEnv
    modNames = Map.keysSet modEnv'

-- | What variables are free in the binding but bound in the broader module
-- context? These variables represent (and are typed as) thunks.
bindingThunks :: Free e => Set Name -> ModuleBinding e -> Set Name
bindingThunks modBinds binding =
  case binding of
    Expression e -> expr e
    VectorReplicate len fill -> Set.insert (name len) (expr fill)
    VectorMap vec transform -> Set.insert (name vec) (expr transform)
    VectorIndex vec idx -> Set.fromList [name vec, name idx] `Set.intersection` modBinds
    ModuleIntro modName modArgs -> Set.fromList (map name modArgs) `Set.intersection` modBinds
    ModuleIndex modThunk field -> Set.singleton (name modThunk)
  where
    expr = exprThunks modBinds

exprThunks :: Free e => Set Name -> e -> Set Name
exprThunks modBinds expr = freeVars expr `Set.intersection` modBinds
