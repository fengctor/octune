module Octune.Types.Env where

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map

import           Data.Text        (Text)

import           Octune.Types.AST

type Env = Map Text

buildASTEnv :: AST a -> Env (AST a)
buildASTEnv (File _ decls) = Map.fromList (fmap envEntryFromDecl decls)
  where
    envEntryFromDecl :: AST a -> (Text, AST a)
    envEntryFromDecl (Decl _ vName binding) = (vName, binding)
    envEntryFromDecl _ = error "Parser should ensure this is a Decl"
buildASTEnv _ = error "Should only call buildEnv on Files"
