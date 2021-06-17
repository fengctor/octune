{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.Types.Core where

import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as Map

import           Data.Text        (Text)

import           Octune.Types.AST

type Env = Map Text AST

-- Resulting from after static analysis
data Core
    = CoreSong Int Core
    | CoreNote Note
    | CoreApp LineFun [Core]
    deriving (Show, Read, Eq)

fromAST :: AST -> Core
fromAST (File decls) = go (env Map.! "main")
  where
    envEntryFromDecl :: AST -> (Text, AST)
    envEntryFromDecl (Decl vName binding) = (vName, binding)
    envEntryFromDecl _ = error "Parser should ensure this is a Decl"

    env :: Env
    env = Map.fromList (fmap envEntryFromDecl decls)

    go :: AST -> Core
    go (Song bpm lineExpr)         = CoreSong bpm (go lineExpr)
    go (Var vName)                 = go (env Map.! vName)
    go (LineNote note)             = CoreNote note
    go (LineApp lineFun lineExprs) = CoreApp lineFun (fmap go lineExprs)
    go _ = error "Arg should only have song or line expressions"
fromAST _ = error "Should only call fromAST on Files"
