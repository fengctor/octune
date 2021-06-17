module Octune.StaticAnalysis where

import           Data.Text                      (Text)

import           Octune.StaticAnalysis.VarUsage
import           Octune.Types

staticAnalysis :: Env AST -> AST -> Either Text ()
staticAnalysis env ast = checkVarsDeclared env ast
