{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           System.Environment
import           System.Exit
import           System.IO

import           Data.Bifunctor
import           Data.Foldable
import           Data.Monoid

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec

import           Data.WAVE

import           Octune.Annotate
import           Octune.CodeGen
import           Octune.NameResolution
import           Octune.Parser
import           Octune.StaticAnalysis
import           Octune.Types


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
    case compile (zip fileNames fileContents) of
        Left errMsg ->
            TIO.hPutStrLn stderr (T.append "error: " errMsg)
            *> exitWith (ExitFailure 1)
        Right (songName, songWAVE) ->
            -- traverse_ (print . length . waveSamples) songWAVEs
            putWAVEFile (songName ++ ".wav") songWAVE
            *> putStr ("Produced:\n" ++ songName)
  where
    compile :: [(String, Text)] -> Either Text (String, WAVE)
    compile nameContentList = do
        asts <- first (T.pack . errorBundlePretty)
                . traverse (uncurry $ runParser pFile)
                $ nameContentList
        let resolvedASTs = fmap resolveModuleVariables asts
        let initEnv = buildASTEnv resolvedASTs

        checkVarsDeclared initEnv
        checkNoVarCycles initEnv
        let beatLenEnv = annotateBeatLengths initEnv
        checkBeatsAssertions beatLenEnv

        mainModule <- findMainModule asts
        mainWAVE <- genWAVE (coreEnv beatLenEnv) mainModule
        -- Song name is the last component of the main module name
        -- TODO: take output song name as an arg
        pure (T.unpack (last mainModule), mainWAVE)

findMainModule :: [AST a] -> Either Text [Text]
findMainModule asts =
    case getFirst . mconcat . fmap findMain $ asts of
        Nothing    -> Left "Must have a module containing `main`"
        Just mName -> pure mName
  where
    findMain :: AST a -> First [Text]
    findMain (File _ moduleName decls) = First $ moduleName <$
        find (\case { Decl _ "main" _ -> True; _ -> False}) decls
    findMain _ = error "Should only be called on Files"
