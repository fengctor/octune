{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.CodeGen.WAVEGen where

import qualified Data.Map.Strict           as Map
import           Data.Text                 (Text)

import           Data.WAVE

import           Octune.CodeGen.SamplesGen
import           Octune.Types

-- Number of frames per second
frameRate :: Int
frameRate = 36000

genWAVE :: Env Core -> Either Text WAVE
genWAVE env =
    case Map.lookup "main" env of
        Just (CoreSong bpm coreExpr) ->
            WAVE header <$> genSamples env bpm frameRate coreExpr
        Just _ ->
            Left "`main` must be a song expression"
        Nothing ->
            Left "Program must contain `main`"
  where
    header :: WAVEHeader
    header =
        WAVEHeader {
            waveNumChannels = 1,
            waveFrameRate = frameRate,
            waveBitsPerSample = 16,
            waveFrames = Nothing
        }