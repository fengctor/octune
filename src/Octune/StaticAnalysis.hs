module Octune.StaticAnalysis where

import           Data.Foldable

import           Data.Text                      (Text)

import           Octune.StaticAnalysis.VarUsage
import           Octune.Types

staticAnalysis :: Env (AST Ann) -> (AST Ann) -> Either Text ()
staticAnalysis env ast =
    traverse_ ($ env) [checkVarsDeclared, checkNoVarCycles]
