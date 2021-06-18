{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.Types.Core where

import           Data.Text         (Text)

import           Octune.Types.AST
import           Octune.Types.Env
import           Octune.Types.Note

-- Resulting from after static analysis
data Core
    = CoreSong !Int Core
    | CoreVar Text
    | CoreNote Note
    | CoreApp !LineFun [Core]
    deriving (Show, Read, Eq)

coreEnv :: Env (AST a) -> Env Core
coreEnv = fmap go
  where
    go :: AST a -> Core
    go (Song _ bpm lExpr)      = CoreSong bpm (go lExpr)
    go (Var _ vName)           = CoreVar vName
    go (LineNote _ note)       = CoreNote note
    go (LineApp _ lFun lExprs) = CoreApp lFun (go <$> filter keep lExprs)
    go _ = error "Arg should only have song or line expressions"

    keep :: AST a -> Bool
    keep BeatsAssertion{} = False
    keep _                = True
