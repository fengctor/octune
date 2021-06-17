{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.StaticAnalysis.VarUsage where

import           Data.Foldable

import qualified Data.Map.Strict  as Map

import           Data.Text        (Text)

import           Octune.Types.AST
import           Octune.Types.Env

-- Checks that variables used have all been declared
checkVarsDeclared :: Env AST -> AST -> Either Text ()
checkVarsDeclared env = go
  where
    go :: AST -> Either Text ()
    go (File decls)      = traverse_ go decls
    go (Decl vName expr) = checkDeclRhs vName expr
    go _                 = error "Should be handled by checkDeclRhs"

    checkDeclRhs :: Text -> AST -> Either Text ()
    checkDeclRhs declName (Song _ expr) =
        checkDeclRhs declName expr
    checkDeclRhs declName (Var vName) =
        case Map.lookup vName env of
            Nothing ->
                Left $ mconcat
                    [ "Undefined variable `"
                    , vName
                    , "` in declaration of `"
                    , declName
                    , "`"
                    ]
            Just _ ->
                pure ()
    checkDeclRhs _ (LineNote _) =
        pure ()
    checkDeclRhs declName (LineApp _ args) =
        traverse_ (checkDeclRhs declName) args
    checkDeclRhs _ _ = error "Should not have File or Decl from parsing"
