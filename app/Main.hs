{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
module Main where

--- Example from https://github.com/BartMassey/wave/blob/master/writetest.hs
--- Write a 1-second 48Ksps 240Hz half-amplitude mono square
--- wave into "square.wav".

import           Data.Bits
import           Data.Foldable
import           Data.List

import           Data.Text       (Text)
import qualified Data.Text       as T
import qualified Data.Text.IO    as TIO

import           System.IO

import           Text.Megaparsec

import           Data.WAVE

import           Octune.AST
import           Octune.Parser
import           Octune.WaveGen


main :: IO ()
main = do
    let twinkleFile = "twinkleTwinkle.otn"
    twinkleContents <- TIO.readFile ("test/otn_files/" ++ twinkleFile)
    case runParser pFile twinkleFile twinkleContents of
        Right twinkleTwinkle ->
            testSongs
                ["pokemonThing.wav", "twinkleTwinkle.wav"]
                [pokemonThing, twinkleTwinkle]
        Left todo ->
            hPutStrLn stderr "Parse error: TODO"

testSongs :: [String] -> [AST] -> IO ()
testSongs fileNames songs =
    case traverse genWAVE songs of
        Left errMsg ->
            hPutStrLn stderr $ T.unpack errMsg
        Right songWAVEs -> do
            traverse_ (uncurry putWAVEFile) $ zip fileNames songWAVEs
            putStrLn "Done"

pokemonThing :: AST
pokemonThing = File [Decl "main" (Song 120 (LineApp Merge [left,right]))]
  where
    right = Line
        [ Note 1 Rest
        , Note 0.5 (Sound A Nothing 4)
        , Note 1 (Sound B Nothing 4)
        , Note 0.5 (Sound D Nothing 5)
        , Note 1.5 (Sound E Nothing 5)
        , Note 1.5 (Sound F (Just Sharp) 5)
        , Note 0.5 (Sound A Nothing 5)
        , Note 5.5 (Sound E Nothing 5)
        , Note 0.5 (Sound F Nothing 5)
        , Note 1 (Sound F (Just Sharp) 5)
        , Note 0.5 (Sound E Nothing 5)
        , Note 0.5 (Sound F Nothing 5)
        , Note 0.5 (Sound F (Just Sharp) 5)
        , Note 1.5 (Sound A (Just Sharp) 5)
        , Note 1.5 (Sound C (Just Sharp) 6)
        , Note 0.5 (Sound B Nothing 5)
        , Note 0.5 (Sound F (Just Sharp) 5)
        , Note 0.5 (Sound F Nothing 5)
        , Note 4.5 (Sound F (Just Sharp) 5)
        ]
    left = LineApp Seq
        [ Line
            [ Note 1 (Sound D Nothing 3)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound B Nothing 3)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound E Nothing 3)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound B Nothing 3)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound F (Just Sharp) 3)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound B Nothing 3)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound A Nothing 2)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound B Nothing 3)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound F (Just Sharp) 3)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound B Nothing 3)
            , Note 0.5 (Sound A Nothing 3)
            , Note 1 (Sound F (Just Sharp) 3)
            , Note 0.5 (Sound A (Just Sharp) 3)
            , Note 1 (Sound C (Just Sharp) 4)
            , Note 0.5 (Sound A (Just Sharp) 3)
            ]
        , LineApp (Repeat 2)
            [ Line
                [ Note 1 (Sound B Nothing 2)
                , Note 0.5 (Sound A Nothing 3)
                , Note 1 (Sound B Nothing 3)
                , Note 0.5 (Sound A Nothing 3)
                ]
            ]
        ]

