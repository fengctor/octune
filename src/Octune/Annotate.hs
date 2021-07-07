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
    go s@(Song _ _ expr) =
        s & _Song . _3
          .~ annotatedExpr
          & annotation . beatLength
          .~ annotatedExpr ^. annotation . beatLength
      where
        annotatedExpr = memoAnnotate expr
    go v@(Var _ vName) =
        v & annotation . beatLength
          .~ memoAnnotate (env Map.! vName) ^. annotation . beatLength
    go n@(LineNote _ (Note _ beats _)) =
        n & annotation . beatLength
          ?~ beats
    go a@(LineApp _ lFun args) =
        a & _LineApp . _3
          .~ annotatedArgs
          & annotation . beatLength
          ?~ foldlOf'
                 (traversed . annotation . beatLength . _Just)
                 (\acc cur -> combineFun acc (beatMod cur))
                 0
                 annotatedArgs
      where
        annotatedArgs = fmap memoAnnotate args
        (combineFun, beatMod) =
            case lFun of
                Seq      -> ((+), id)
                Merge    -> (max, id)
                Repeat n -> ((+), (* toRational n))
                Volume _ -> ((+), id)
    -- Beats assertions do not take up time
    go b@BeatsAssertion{} = b
    go _ = error "Should not have File or Decl from parsing"
