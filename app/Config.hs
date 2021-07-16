module Config where

import           Options.Applicative

data Config
    = Config
        { output    :: Maybe String
        , onlyCheck :: Bool
        , cores     :: Int
        , files     :: [String]
        }
    deriving Show

config :: Parser Config
config =
    Config
    <$> optional (
            strOption
            ( long "output"
           <> short 'o'
           <> help "Name of the output song"
           <> metavar "NAME" ))
    <*> switch
        ( long "check"
       <> help "Check that song is valid without producing the WAV" )
    <*> option auto
        ( short 'j'
       <> help "Number of cores to run on"
       <> showDefault
       <> value 2
       <> metavar "INT")
    <*> some (argument str (metavar "FILES..."))
