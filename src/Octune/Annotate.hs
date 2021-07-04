module Octune.Annotate where

import qualified Data.Map.Strict as Map

import           Control.Lens

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
    go (Song ann bpm expr) =
        Song newAnnotation bpm annotatedExpr
      where
        annotatedExpr = memoAnnotate expr
        thisBeatLength = annotatedExpr ^. annotation . beatLength
        newAnnotation = ann & beatLength .~ thisBeatLength
    go (Var ann vName) =
        Var newAnnotation vName
      where
        annotatedExpansion = memoAnnotate (env Map.! vName)
        thisBeatLength = annotatedExpansion ^. annotation . beatLength
        newAnnotation = ann & beatLength .~ thisBeatLength
    go (LineNote ann note@(Note _ beats _)) =
        LineNote newAnnotation note
      where
        newAnnotation = ann & beatLength ?~ beats
    go (LineApp ann lFun args) =
        LineApp newAnnotation lFun annotatedArgs
      where
        annotatedArgs = fmap memoAnnotate args
        (combineFun, beatFun) =
            case lFun of
                Seq      -> ((+), id)
                Merge    -> (max, id)
                Repeat n -> ((+), (* toRational n))
                Volume _ -> ((+), id)
        thisBeatLength =
            foldlOf'
                (traversed . annotation . beatLength . _Just)
                (\a c -> combineFun a (beatFun c))
                0
                annotatedArgs
        newAnnotation = ann & beatLength ?~ thisBeatLength
    -- Beats assertions do not take up time
    go b@BeatsAssertion{} = b
    go _ = error "Should not have File or Decl from parsing"
