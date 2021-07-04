{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.StaticAnalysis.BarBeats where

import           GHC.Real            (Ratio (..))

import           Control.Monad

import           Data.Foldable

import           Control.Lens

import           Data.Text           (Text)
import qualified Data.Text           as T

import           Text.Megaparsec.Pos

import           Octune.Types

checkBeatsAssertions :: Env (AST Ann) -> Either Text ()
checkBeatsAssertions env = traverse_ go env
  where
    go :: AST Ann -> Either Text ()
    go (Song _ _ expr)  = go expr
    go Var{}            = pure ()
    go LineNote{}       = pure ()
    go BeatsAssertion{} = pure ()
    go (LineApp _ lFun args) = do
        when (hasBeatsAssertionsArgs lFun) $
            checkBeatsList args
        traverse_ go args
    go _ = error "Should only have Song and Line expressions in Env"

    hasBeatsAssertionsArgs :: LineFun -> Bool
    hasBeatsAssertionsArgs Merge = False
    hasBeatsAssertionsArgs _     = True

    -- TODO: figure out how to build a traversal focusing on "bars"
    --       paired with their expected beats
    checkBeatsList :: [AST Ann] -> Either Text ()
    checkBeatsList [] = pure ()
    checkBeatsList (BeatsAssertion ann mBeats : es) =
        let (curBar, nextBar) = span notBeatsAssertion es
            curBarLength =
                sumOf (traversed . annotation . beatLength . _Just) curBar
         in case mBeats of
                Nothing -> checkBeatsList nextBar
                Just beats
                  | beats == curBarLength -> checkBeatsList nextBar
                  | otherwise ->
                      Left . T.pack $ mconcat
                          [ ann ^. pos . to sourcePosPretty
                          , ":"
                          , "\n    - Beat assertion failure"
                          , "\n      Expected beats: "
                          , showRational beats
                          , "\n        Actual beats: "
                          , showRational curBarLength
                          ]
    checkBeatsList es = checkBeatsList (dropWhile notBeatsAssertion es)

    notBeatsAssertion :: AST Ann -> Bool
    notBeatsAssertion BeatsAssertion{} = False
    notBeatsAssertion _                = True

    showRational :: Rational -> String
    showRational (n :% 1) = show n
    showRational (n :% d) = show n ++ "/" ++ show d
