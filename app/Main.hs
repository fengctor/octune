{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}

module Main where

import           System.Exit
import           System.IO

import           Control.Monad

import           Data.Bifunctor
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid

import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO

import           Text.Megaparsec

import           Data.WAVE

import           Options.Applicative

import           Octune.Annotate
import           Octune.CodeGen
import           Octune.NameResolution
import           Octune.Parser
import           Octune.StaticAnalysis
import           Octune.Types

import           Config


main :: IO ()
main = runOctune =<< execParser opts
  where
    opts =
        info
            (config <**> helper)
            ( fullDesc
           <> progDesc "Produce a WAV file from Octune files"
           <> header "Octune - a DSL for creating 8-bit style music" )

-- Currently compiles exactly 1 song per file
runOctune :: Config -> IO ()
runOctune cfg = do
    let fileNames = files cfg
    fileContents <- traverse TIO.readFile fileNames
    case fileContents of
        [] -> hPutStrLn stderr "Error: no files provided"
              *> exitWith (ExitFailure 1)
        _  -> pure ()

    let checkResult = checkFiles (zip fileNames fileContents)
    (checkedEnv, mainModule) <- exitOnError checkResult

    when (onlyCheck cfg) $
        putStrLn "No issues found." *> exitSuccess

    songWAVE <- exitOnError $ genWAVE (coreEnv checkedEnv) mainModule
    let defaultSongName = T.unpack (last mainModule)
    let songName = fromMaybe defaultSongName (output cfg)
    putWAVEFile (songName ++ ".wav") songWAVE
    putStrLn ("Produced: " ++ songName)

exitOnError :: Either Text b -> IO b
exitOnError (Right b) = pure b
exitOnError (Left errMsg) =
    TIO.hPutStrLn stderr ("error: " <> errMsg)
    *> exitWith (ExitFailure 1)

-- Returns the checked environment and main moduile used for producing the WAV
checkFiles :: [(String, Text)] -> Either Text (Env (AST Ann), [Text])
checkFiles nameContentList = do
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
    pure (beatLenEnv, mainModule)

-- TODO: error if multiple `main`s?
findMainModule :: [AST a] -> Either Text [Text]
findMainModule asts =
    case getFirst . foldMap findMain $ asts of
        Nothing    -> Left "Must have a module containing `main`"
        Just mName -> pure mName
  where
    findMain :: AST a -> First [Text]
    findMain (File _ moduleName decls) = First $ moduleName <$
        find (\case { Decl _ "main" _ -> True; _ -> False}) decls
    findMain _ = error "Should only be called on Files"
