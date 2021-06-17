{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TupleSections        #-}

module Octune.StaticAnalysis.VarUsage where

import           Data.Foldable

import           Data.Graph       (Graph)
import qualified Data.Graph       as Graph
import qualified Data.Map.Strict  as Map

import           Data.Text        (Text)
import qualified Data.Text        as T

import           Octune.Types.AST
import           Octune.Types.Env

-- Checks that variables used have all been declared
checkVarsDeclared :: Env AST -> Either Text ()
checkVarsDeclared env = traverse_ (uncurry checkDeclRhs) (Map.toList env)
  where
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

-- Checks that usages of variables don't form a cycle
checkNoVarCycles :: Env AST -> Either Text ()
checkNoVarCycles env = errorOnSelfEdges *> errorOnCycles
  where
    edgesFromVar :: Text -> AST -> (Text, Text, [Text])
    edgesFromVar v expr = (v, v, variablesIn expr)

    variablesIn :: AST -> [Text]
    variablesIn (Song _ expr)    = variablesIn expr
    variablesIn (Var v)          = [v]
    variablesIn (LineNote _)     = []
    variablesIn (LineApp _ args) = foldl' (\a c -> variablesIn c ++ a) [] args
    variablesIn _ = error "Should not have File or Decl from parsing"

    -- Graph from variables to the variables that appear in their declaration
    varGraph :: Graph
    (varGraph, varNodeFromVertex, _) =
        Graph.graphFromEdges $ fmap (uncurry edgesFromVar) (Map.toList env)

    varFromVertex :: Graph.Vertex -> Text
    varFromVertex vertex = let (v,_,_) = varNodeFromVertex vertex in v

    errorOnSelfEdges :: Either Text ()
    errorOnSelfEdges =
        case filter (uncurry (==)) (Graph.edges varGraph) of
            [] -> pure ()
            cs ->
                Left $ mconcat
                    [ "Variables cannot reference themselves:\n"
                    , T.unlines $ fmap (T.append "\t- ") badVars
                    ]
                  where
                    showVar = denoteVar . varFromVertex . fst
                    badVars = fmap showVar cs
    errorOnCycles :: Either Text ()
    errorOnCycles =
        case filter ((> 1) . length) (Graph.scc varGraph) of
            [] -> pure ()
            cs ->
                Left $ mconcat
                    [ "Variable usages cannot form a cycle:\n"
                    , T.unlines $ fmap (T.append "\t- ") badComponents
                    ]
                  where
                    showComponent component =
                        let cycleVars@(v:_) =
                                fmap
                                    (denoteVar . varFromVertex)
                                    (toList component)
                         in T.intercalate " -> " (cycleVars ++ [v])
                    badComponents = fmap showComponent cs

    denoteVar :: Text -> Text
    denoteVar var = mconcat ["`",var,"`"]
