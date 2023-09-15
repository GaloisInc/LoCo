module Language.Optimal.Typecheck where

import Data.Map qualified as Map
import Data.Text qualified as Text
import Language.Optimal.Syntax (Env, ModuleDecl (..), Type (..))

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

-- | Check whether module declarations have an arity matching their type
-- signature
checkArity :: ModuleDecl -> Either String ()
checkArity ModuleDecl {..} = check modTy modParams
  where
    check ty params =
      case (ty, params) of
        (Rec _ _, []) -> pure ()
        (Rec _ _, _) -> too "many"
        (Arrow _ _, []) -> too "few"
        (Arrow _ t2, _ : ps) -> check t2 ps
        _ -> Left (unwords ["module", modName', "had non-record type"])
    too what =
      Left $
        unwords
          ["module", modName', "declared with too", what, "parameters"]
    modName' = unwords ["\"", Text.unpack modName, "\""]
