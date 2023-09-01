module Language.Optimal.Typecheck where

import Data.Map qualified as Map
import Language.Optimal.Syntax

-- | For modules declared with a type alias, inline the types in the alias to
-- the greatest extent possible in the given type declaration environment
expandType :: [TypeDecl] -> ModuleDecl -> Type
expandType tyDecls ModuleDecl {..} = expandTy modTy
  where
    expandTy ty =
      case ty of
        Alias alias -> maybe ty expandTy (tyEnv Map.!? alias)
        Arrow t1 t2 -> Arrow t1 $ expandTy t2
        Tuple ts -> Tuple $ map expandTy ts
        List t -> List $ expandTy t
        Rec env -> Rec $ expandTy <$> env
    tyEnv = Map.fromList [(tdName, tdType) | TypeDecl {..} <- tyDecls]

checkModule :: ModuleDecl -> Either String ModuleDecl
checkModule ModuleDecl {..} = undefined
