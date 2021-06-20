module Octune.Types.Env where

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map

import           Octune.Types.AST

type Env = Map QualifiedName

-- TODO: normalize bare variables in decls by mapping name -> (name, module)
buildASTEnv :: [AST a] -> Env (AST a)
buildASTEnv asts = Map.fromList $ envBindingList =<< asts
  where
    envBindingList :: AST a -> [(QualifiedName, AST a)]
    envBindingList (File _ moduleName decls) = fmap envEntryFromDecl decls
      where
        envEntryFromDecl :: AST a -> (QualifiedName, AST a)
        envEntryFromDecl (Decl _ vName binding) =
            (QualName moduleName vName, binding)
        envEntryFromDecl _ = error "Parser should ensure this is a Decl"
    envBindingList _ = error "Should only be called on Files"
