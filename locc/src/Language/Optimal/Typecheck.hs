module Language.Optimal.Typecheck where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Language.Haskell.TH (Name)
import Language.LoCo.Toposort (topoSortPossibly)
import Language.Optimal.Compile.Haskell.Free (Free, freeVars)
import Language.Optimal.Syntax
import Language.Optimal.Util (Named (name))

-- | For modules declared with a type alias, try to replace the alias with the
-- full type it names.
expandTypes :: [TypeDecl] -> [ModuleDecl] -> [ModuleDecl]
expandTypes tyDecls = map retype
  where
    retype ModuleDecl {..} = ModuleDecl {modExpandedTy = expandType modOriginalTy, ..}
    expandType ty =
      case ty of
        Alias alias -> maybe ty expandType (tyEnv Map.!? alias)
        Arrow t1 t2 -> Arrow t1 $ expandType t2
        Tuple ts -> Tuple $ map expandType ts
        List t -> List $ expandType t
        Rec env -> Rec $ expandType <$> env
    tyEnv = Map.fromList [(tdName, tdType) | TypeDecl {..} <- tyDecls]

checkModule :: ModuleDecl -> Either String ModuleDecl
checkModule ModuleDecl {..} = undefined

sortModuleBindings ::
  (Free e, MonadFail m) =>
  Map Name (ModuleBinding e) ->
  m [(Name, ModuleBinding e)]
sortModuleBindings modEnv =
  do
    let dependencies =
          [ (var, deps)
            | (var, binding) <- Map.toList modEnv,
              let deps = Set.toList (bindingThunks modNames binding)
          ]
    orderedNames <- reverse <$> topoSortPossibly dependencies
    pure [(n, modEnv Map.! n) | n <- orderedNames]
  where
    modNames = Map.keysSet modEnv

-- | What variables are free in the binding but bound in the broader module
-- context?
bindingThunks :: Free e => Set Name -> ModuleBinding e -> Set Name
bindingThunks modBinds binding =
  case binding of
    ValueBinding e -> exprThunks modBinds e
    VectorBinding len fill -> Set.insert (name len) (expr fill)
    IndexBinding vec idx -> Set.fromList [name vec, name idx]
  where
    expr = exprThunks modBinds

exprThunks :: Free e => Set Name -> e -> Set Name
exprThunks modBinds expr = freeVars expr `Set.intersection` modBinds
