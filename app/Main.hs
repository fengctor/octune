{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Main where

import           System.Environment
import           System.Exit
import           System.IO

import           Control.Monad

import           Data.Bifunctor
import           Data.Foldable

import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.IO       as TIO


import           Text.Megaparsec

import           Data.WAVE

import           Octune.Parser
import           Octune.Types.Core
import           Octune.WaveGen


main :: IO ()
main = runOctune

-- Currently compiles exactly 1 song per file
runOctune :: IO ()
runOctune = do
    fileNames <- getArgs
    fileContents <- traverse TIO.readFile fileNames
    case fileContents of
        [] -> TIO.hPutStrLn stderr "Error: no files provided"
              *> exitWith (ExitFailure 1)
        _  -> pure ()
    case traverse (uncurry compile) (zip fileNames fileContents) of
        Left errMsg ->
            TIO.hPutStrLn stderr errMsg
            *> exitWith (ExitFailure 1)
        Right songWAVEs ->
            -- traverse_ (print . length . waveSamples) songWAVEs
            traverse_ (uncurry putWAVEFile) (zip songNames songWAVEs)
            *> putStr ("Produced:\n" ++ unlines songNames)
              where
                songNames :: [String]
                songNames = fmap ((++ ".wav") . takeWhile (/= '.')) fileNames
  where
    compile :: String -> Text -> Either Text WAVE
    compile fileName =
        genWAVE . fromAST <=<
            first (T.pack . errorBundlePretty)
            . runParser pFile fileName
