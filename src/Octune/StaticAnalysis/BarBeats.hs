{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.StaticAnalysis.BarBeats where

import           GHC.Real            (Ratio (..))

import           Control.Monad

import           Data.Foldable

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

    checkBeatsList :: [AST Ann] -> Either Text ()
    checkBeatsList [] = pure ()
    checkBeatsList (BeatsAssertion ann mBeats : es) =
        let (curBar, nextBar) = span notBeatsAssertion es
            foldFun acc = fmap (+ acc) . beatLength . getAug
            mCurBarLength = foldM foldFun 0 curBar
         in case (mBeats, mCurBarLength) of
                (Nothing, _) -> checkBeatsList nextBar
                (Just beats, Just curBarLength)
                  | beats == curBarLength -> checkBeatsList nextBar
                  | otherwise ->
                      Left . T.pack $ mconcat
                          [ sourcePosPretty (pos ann)
                          , ":"
                          , "\n    - Beat assertion failure"
                          , "\n      Expected beats: "
                          , showRational beats
                          , "\n        Actual beats: "
                          , showRational curBarLength
                          ]
                _ -> error "Should not have Nothing beatLength in line-exprs"
    checkBeatsList es = checkBeatsList (dropWhile notBeatsAssertion es)

    notBeatsAssertion :: AST Ann -> Bool
    notBeatsAssertion BeatsAssertion{} = False
    notBeatsAssertion _                = True

    showRational :: Rational -> String
    showRational (n :% 1) = show n
    showRational (n :% d) = show n ++ "/" ++ show d
