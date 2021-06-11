{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Main where

--- Example from https://github.com/BartMassey/wave/blob/master/writetest.hs
--- Write a 1-second 48Ksps 240Hz half-amplitude mono square
--- wave into "square.wav".

import           Data.Bits
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.Text       (Text)
import qualified Data.Text       as Text

import           System.IO

import           Data.WAVE

import           Octune.AST
import           Octune.WaveGen

main :: IO ()
main =
    testSongs
        ["pokemonThing.wav", "twinkleTwinkle.wav"]
        [pokemonThing, twinkleTwinkle]

testSongs :: [String] -> [Env] -> IO ()
testSongs fileNames songs =
    case traverse genWAVE songs of
        Left errMsg ->
            hPutStrLn stderr $ Text.unpack errMsg
        Right songWAVEs -> do
            traverse (uncurry putWAVEFile) $ zip fileNames songWAVEs
            putStrLn "Done"

twinkleTwinkle :: Env
twinkleTwinkle =
    Map.fromList [("main", Song 60 (LineApp Merge [right, left]))]
  where
    right = Line
        [ Note (Sound C Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound C Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound G Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound G Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound A Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound A Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound G Nothing 4) 0.4
        , Note Rest 0.1
        , Note Rest 0.5
        , Note (Sound F Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound F Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound E Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound E Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound D Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound D Nothing 4) 0.4
        , Note Rest 0.1
        , Note (Sound C Nothing 4) 0.4
        ]
    left = Line
        [ Note (Sound C Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25
        , Note (Sound E Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25

        , Note (Sound C Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25
        , Note (Sound E Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25

        , Note (Sound C Nothing 3) 0.25, Note (Sound A Nothing 3) 0.25
        , Note (Sound F Nothing 3) 0.25, Note (Sound A Nothing 3) 0.25

        , Note (Sound C Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25
        , Note (Sound E Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25

        , Note (Sound B Nothing 2) 0.25, Note (Sound G Nothing 3) 0.25
        , Note (Sound D Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25

        , Note (Sound C Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25
        , Note (Sound E Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25

        , Note (Sound D Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25
        , Note (Sound F Nothing 3) 0.25, Note (Sound G Nothing 3) 0.25

        , Note (Sound C Nothing 3) 1
        ]

pokemonThing :: Env
pokemonThing = Map.fromList [("main", Song 120 right)]
  where
    right = Line
        [ Note (Sound A Nothing 3) 0.5
        , Note (Sound B Nothing 3) 1
        , Note (Sound D Nothing 4) 0.5
        , Note (Sound E Nothing 4) 1.5
        , Note (Sound F (Just Sharp) 4) 1.5
        , Note (Sound A Nothing 4) 0.5
        , Note (Sound E Nothing 4) 5.5
        , Note (Sound F Nothing 4) 0.5
        , Note (Sound F (Just Sharp) 4) 1
        , Note (Sound E Nothing 4) 0.5
        , Note (Sound F Nothing 4) 0.5
        , Note (Sound F (Just Sharp) 4) 0.5
        , Note (Sound A (Just Sharp) 4) 1.5
        , Note (Sound C (Just Sharp) 5) 1.5
        , Note (Sound B Nothing 4) 0.5
        , Note (Sound F (Just Sharp) 4) 0.5
        , Note (Sound F Nothing 4) 0.5
        , Note (Sound F (Just Sharp) 4) 4.5
        ]
