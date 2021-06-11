{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.WaveGen where

import           Data.Bits
import           Data.Int
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as Text

import           Data.WAVE

import           Octune.AST

type Env = Map Text AST

-- Number of frames per second
frameRate :: Int32
frameRate = 48000

-- Default amplitude of a wave
amplitude :: Int32
amplitude = 1 `shiftL` 28

-- Layer a list of samples over each other
mergeSamples :: [WAVESamples] -> WAVESamples
mergeSamples = foldl1' (zipWith (zipWith (+)))


genMainSamples :: Env -> Either Text WAVESamples
genMainSamples env =
    case Map.lookup "main" env of
        Nothing ->
            Left "No `main` melody found"
        Just (Song bpm lineExpr) ->
            genSamples env bpm lineExpr
        _ ->
            Left "`main` must be a song"

-- Line Expressions
genSamples :: Env -> Int -> AST -> Either Text WAVESamples
genSamples env bpm ast = go ast
  where
    go :: AST -> Either Text WAVESamples
    go (Var v) =
        case Map.lookup v env of
            Nothing ->
                Left $ mconcat
                    [ "Undefined variable `"
                    , v
                    , "`"
                    ]
            Just lineExpr ->
                go lineExpr
    go (Line noteRow) =
        pure $ noteRow >>= noteToSamples bpm

noteToSamples :: Int -> Note -> WAVESamples
noteToSamples bpm (Note pitch beats) =
    let durationFrames =
            toRational bpm * beats * toRational frameRate / 60
     in take (round durationFrames)
        . mconcat
        . repeat
        $ pitchWave pitch

-- Sample line constituting a single wavelength of the pitch.
-- frameRate / frequency = wavelength in frames
pitchWave :: Pitch -> WAVESamples
pitchWave Rest                = [[0]]
pitchWave (Sound _ _ n)
  | n < 0 || n > 8 = [[0]] -- TODO: return a Left?
pitchWave (Sound C Nothing 4) =
    let frequency = 262
        halfWaveFrames = fromEnum $ div (div frameRate 2) frequency
     in mconcat
            [ replicate halfWaveFrames [-amplitude]
            , replicate halfWaveFrames [amplitude]
            ]
pitchWave _ = error "TODO"

genWAVE :: Env -> Either Text WAVE
genWAVE = undefined
