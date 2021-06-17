{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.Types.Core where

import           Data.Text        (Text)

import           Octune.Types.AST
import           Octune.Types.Env

-- Resulting from after static analysis
data Core
    = CoreSong !Int Core
    | CoreVar Text
    | CoreNote Note
    | CoreApp !LineFun [Core]
    deriving (Show, Read, Eq)

coreEnv :: AST -> Env Core
coreEnv ast = fmap go (buildASTEnv ast)
  where
    go :: AST -> Core
    go (Song bpm lineExpr)         = CoreSong bpm (go lineExpr)
    go (Var vName)                 = CoreVar vName
    go (LineNote note)             = CoreNote note
    go (LineApp lineFun lineExprs) = CoreApp lineFun (fmap go lineExprs)
    go _ = error "Arg should only have song or line expressions"
