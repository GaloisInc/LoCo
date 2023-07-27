module Language.Optimal.Typecheck where

import Data.Map qualified as Map
import Data.Text qualified as Text
import Language.Optimal.Syntax

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

-- expandType md@ModuleDecl {..} =
--   case modExpandedTy of
--     Alias alias ->
--     Rec _ -> Right md
--     Arrow t1 t2 ->
--     _ -> Left $ "cannot assign non-record type " <> show modExpandedTy <> " to module " <> Text.unpack modName
