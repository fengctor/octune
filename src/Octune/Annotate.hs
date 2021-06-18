module Octune.Annotate where

import           Control.Monad

import qualified Data.Map.Strict as Map

import           Octune.Types

annotateBeatLengths :: Env (AST Ann) -> Env (AST Ann)
annotateBeatLengths env = cache
  where
    cache :: Env (AST Ann)
    cache = fmap go env

    memoAnnotate :: AST Ann -> AST Ann
    memoAnnotate (Var _ vName) = cache Map.! vName
    memoAnnotate expr          = go expr

    go :: AST Ann -> AST Ann
    go (Song a bpm expr) =
        Song newAnnotation bpm annotatedExpr
      where
        annotatedExpr = memoAnnotate expr
        thisBeatLength = beatLength (getAug annotatedExpr)
        newAnnotation = a { beatLength = thisBeatLength }
    go (Var a vName) =
        Var newAnnotation vName
      where
        annotatedExpansion = memoAnnotate (env Map.! vName)
        thisBeatLength = beatLength (getAug annotatedExpansion)
        newAnnotation = a { beatLength = thisBeatLength }
    go (LineNote a note@(Note _ beats _)) =
        LineNote newAnnotation note
      where
        newAnnotation = a { beatLength = Just beats }
    go (LineApp a lFun args) =
        LineApp newAnnotation lFun annotatedArgs
      where
        annotatedArgs = fmap memoAnnotate args
        (combineFun, beatFun) =
            case lFun of
                Seq      -> ((+), id)
                Merge    -> (max, id)
                Repeat n -> ((+), (* toRational n))
        foldFun acc = fmap (combineFun acc . beatFun) . beatLength . getAug
        thisBeatLength = foldM foldFun 0 annotatedArgs
        newAnnotation = a { beatLength = thisBeatLength }
    go (BeatsAssertion a mb) =
        BeatsAssertion newAnnotation mb
      where
        newAnnotation = a { beatLength = Just 0 }
    go _ = error "Should not have File or Decl from parsing"
