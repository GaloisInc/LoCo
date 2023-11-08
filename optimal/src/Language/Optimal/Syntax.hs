{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Optimal.Syntax where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Language.Haskell.TH (Exp, Name)
import Language.Optimal.Compile.Haskell.Free (Free (..))
import Language.Optimal.Util

data ModuleDecl e = ModuleDecl
  { modTy :: Type,
    modName :: Symbol,
    modParams :: [Symbol],
    modEnv :: Env (ModuleBinding e)
  }
  deriving (Eq, Functor, Show)

sequenceModuleDecl :: Applicative f => ModuleDecl (f e) -> f (ModuleDecl e)
sequenceModuleDecl ModuleDecl {..} =
  do
    modEnv' <- traverse sequenceModuleBinding modEnv
    pure ModuleDecl {modEnv = modEnv', ..}

data ModuleBinding e
  = Expression e
  | Value e
  | VectorReplicate Symbol e -- vector introduction
  | VectorGenerate Symbol e -- vector introduction
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
  deriving (Eq, Functor, Show)

sequenceModuleBinding :: Applicative f => ModuleBinding (f a) -> f (ModuleBinding a)
sequenceModuleBinding binding =
  case binding of
    Expression e -> Expression <$> e
    Value e -> Value <$> e
    VectorReplicate s e -> VectorReplicate s <$> e
    VectorGenerate s e -> VectorGenerate s <$> e
    VectorMap s e -> VectorMap s <$> e
    VectorIndex s1 s2 -> pure (VectorIndex s1 s2)
    ModuleIntro mdl args -> pure (ModuleIntro mdl args)
    ModuleIndex mdl field -> pure (ModuleIndex mdl field)

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
    Expression e -> go e
    Value e -> go e
    VectorReplicate len fill -> go (name len) <> go fill
    VectorGenerate len fill -> go (name len) <> go fill
    VectorMap vec transform -> go (name vec) <> go transform
    VectorIndex vec idx -> go (name vec) <> go (name idx)
    ModuleIntro modName modArgs -> foldMap (go . name) modArgs
    ModuleIndex modThunk field -> go (name modThunk)
  where
    go x = freeVars x `Set.intersection` modBinds
