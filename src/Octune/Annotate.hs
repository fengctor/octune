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
    memoAnnotate v@(Var _ vName) =
        v & annotation . beatLength
          .~ (cache Map.! vName) ^. annotation . beatLength
    memoAnnotate expr            = go expr

    go :: AST Ann -> AST Ann
    go e@(Song _ _ expr) =
        e & _Song . _3
          .~ annotatedExpr
          & annotation . beatLength
          .~ annotatedExpr ^. annotation . beatLength
      where
        annotatedExpr = memoAnnotate expr
    go e@(Var _ vName) =
        e & annotation . beatLength
          .~ memoAnnotate (env Map.! vName) ^. annotation . beatLength
    go e@(LineNote _ (Note _ beats _)) =
        e & annotation . beatLength
          ?~ beats
    go e@(LineApp _ lFun args) =
        e & _LineApp . _3
          .~ annotatedArgs
          & annotation . beatLength
          ?~ adjustFun (foldlOf'
                 (traversed . annotation . beatLength . _Just)
                 combineFun
                 0
                 annotatedArgs)
      where
        annotatedArgs = fmap memoAnnotate args
        (combineFun, adjustFun) =
            case lFun of
                Seq             -> ((+), id)
                Merge           -> (max, id)
                Repeat n        -> ((+), (* toRational n))
                UsingWaveform _ -> ((+), id)
                Volume _        -> ((+), id)
                Subsection l r  -> ((+), \total -> max 0 (min r total - l))
    -- Beats assertions do not take up time
    go b@BeatsAssertion{} = b
    go _ = error "Should not have File or Decl from parsing"
