module Octune.Types.Env where

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map

import           Data.Text        (Text)

import           Octune.Types.AST

type Env = Map Text

buildASTEnv :: AST -> Env AST
buildASTEnv (File decls) = Map.fromList (fmap envEntryFromDecl decls)
  where
    envEntryFromDecl :: AST -> (Text, AST)
    envEntryFromDecl (Decl vName binding) = (vName, binding)
    envEntryFromDecl _ = error "Parser should ensure this is a Decl"
buildASTEnv _ = error "Should only call buildEnv on Files"
