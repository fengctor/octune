{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.WaveGen where

import           Data.Bits
import           Data.Int
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Text       (Text)
import qualified Data.Text       as T

import           Data.WAVE

import           Octune.AST

type Env = Map Text AST

-- Multiplier for frequency to go up a semitone
semitoneFreqMultiplier :: Rational
semitoneFreqMultiplier = 1.05946309435929

-- Number of frames per second
frameRate :: Int
frameRate = 48000

-- Default amplitude of a wave
amplitude :: Int32
amplitude = 1 `shiftL` 27

-- Layer a list of samples over each other
mergeSamples :: [WAVESamples] -> WAVESamples
mergeSamples = foldl1' (zipWith (zipWith (+)))


genWAVE :: AST -> Either Text WAVE
genWAVE (File decls) =
    WAVE header <$> (
        genMainSamples
        . Map.fromList
        . fmap envEntryFromDecl
        $ decls
    )
  where
    header :: WAVEHeader
    header =
        WAVEHeader {
            waveNumChannels = 1,
            waveFrameRate = frameRate,
            waveBitsPerSample = 16,
            waveFrames = Nothing
        }
    envEntryFromDecl :: AST -> (Text, AST)
    envEntryFromDecl (Decl vName binding) =
        (vName, binding)
    envEntryFromDecl _ =
        error "Parser should ensure this is a Decl"

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
genSamples env bpm = go
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
    go (LineApp lineFun lineArgs) =
        applyLineFun lineFun lineArgs

    applyLineFun :: LineFun -> [AST] -> Either Text WAVESamples
    applyLineFun Seq =
        fmap mconcat . traverse go
    applyLineFun Merge =
        fmap mergeSamples . traverse go
    applyLineFun (Repeat n) =
        fmap (mconcat . replicate n . mconcat) . traverse go

noteToSamples :: Int -> Note -> WAVESamples
noteToSamples bpm (Note beats pitch) =
    let secondsPerBeat = (beats / toRational bpm) * 60
        durationFrames = secondsPerBeat * toRational frameRate
     in take (round durationFrames)
        . mconcat
        . repeat
        $ pitchWave pitch

-- Sample line constituting a single wavelength of the pitch.
-- frameRate / frequency = wavelength in frames
pitchWave :: Pitch -> WAVESamples
pitchWave Rest = [[0]]
pitchWave (Sound _ _ n)
  | n < 0 || n > 8 = [[0]] -- TODO: return a Left?
pitchWave (Sound letter accidental octave) =
     mconcat
         [ replicate halfWaveFrames [-amplitude]
         , replicate halfWaveFrames [amplitude]
         ]
  where
    -- Frequency of `Sound letter Nothing 0`
    -- Obtained from https://en.wikipedia.org/wiki/Piano_key_frequencies
    baseFrequency :: Rational
    baseFrequency =
        case letter of
            C -> 16.35160
            D -> 18.35405
            E -> 20.60172
            F -> 21.82676
            G -> 24.49971
            A -> 27.50000
            B -> 30.86771

    accidentalMultiplier :: Rational
    accidentalMultiplier =
        case accidental of
            Nothing    -> 1
            Just Flat  -> 1 / semitoneFreqMultiplier
            Just Sharp -> semitoneFreqMultiplier

    frequency :: Rational
    frequency =
        accidentalMultiplier * baseFrequency * product (replicate octave 2)

    halfWaveFrames :: Int
    halfWaveFrames =
        fromEnum $
            (toRational frameRate / frequency) / 2

