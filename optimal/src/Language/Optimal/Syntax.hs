{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# HLINT ignore "Use newtype instead of data" #-}

module Language.Optimal.Syntax where

import Data.List
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Language.Haskell.TH (Name)
import Language.Optimal.Compile.Free (Free (..))
import Language.Optimal.Util

data ModuleDecl e = ModuleDecl
  { modTy :: Type,
    modName :: Symbol,
    modParams :: [Symbol],
    modEnv :: Map (Pattern Symbol) (ModuleBinding e)
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

data Pattern s
  = Sym s
  | Tup [s] -- or [Pattern]
  deriving (Eq, Functor, Ord, Show)

patSyms :: Pattern s -> [s]
patSyms pat =
  case pat of
    Sym s -> [s]
    Tup ss -> ss

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
  | App Type Type
  deriving (Eq, Show)

type Symbol = Text

type Env a = Map Symbol a

--------------------------------------------------------------------------------

sortModuleBindings ::
  (Free e, MonadFail m) =>
  Map (Pattern Symbol) (ModuleBinding e) ->
  m [(Pattern Name, ModuleBinding e)]
sortModuleBindings modEnv =
  do
    let dependencies =
          [ (name <$> pat, map Sym deps)
            | (pat, binding) <- Map.toList modEnv,
              let deps = Set.toList (bindingThunks modNames binding)
          ]
    orderedNames <- reverse <$> topoSortPatterns dependencies
    pure [(n, modEnv' Map.! n) | n <- orderedNames]
  where
    modPats = Map.keys modEnv
    modSyms = concatMap patSyms modPats
    modNames = Set.fromList (map name modSyms)
    modEnv' = Map.mapKeys (fmap name) modEnv

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
    ModuleIntro _modName modArgs -> foldMap (go . name) modArgs
    ModuleIndex modThunk _field -> go (name modThunk)
  where
    go x = freeVars x `Set.intersection` modBinds

topoSortPatterns :: (MonadFail m, Eq n, Show n) => [(Pattern n, [Pattern n])] -> m [Pattern n]
topoSortPatterns = topoSortPossibly refersTo
  where
    refersTo p1 p2 =
      case (p1, p2) of
        (Sym s1, Sym s2) -> s1 == s2
        (Tup ss, Sym s) -> s `elem` ss
        -- Not sure these can come up in practice - for `p2` to be a tuple would
        -- mean we decided that a module binding depended on a tuple, but we
        -- only ever expect to see `Sym` dependencies
        (Sym _, Tup _) -> False
        (Tup ss, Tup ss') -> not (null (ss `intersect` ss'))
