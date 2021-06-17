{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.WaveGen where

import           Data.Bits
import           Data.Int
import           Data.List
import           Data.Text    (Text)

import           Data.WAVE

import           Octune.Types

-- Multiplier for frequency to go up a semitone
semitoneFreqMultiplier :: Rational
semitoneFreqMultiplier = 1.05946309435929

-- Number of frames per second
frameRate :: Int
frameRate = 36000

-- Default amplitude of a wave
amplitude :: Int32
amplitude = 1 `shiftL` 27

{-# INLINE [0] zipWithHom #-}
zipWithHom :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithHom f = go
  where
    go [] ys         = ys
    go xs []         = xs
    go (x:xs) (y:ys) = f x y : go xs ys

-- Layer a list of samples over each other
mergeSamples :: [WAVESamples] -> WAVESamples
mergeSamples = foldl1' (zipWithHom (zipWithHom (+)))

genWAVE :: Core -> Either Text WAVE
genWAVE (CoreSong bpm coreExpr) =
    WAVE header <$> genSamples bpm coreExpr
  where
    header :: WAVEHeader
    header =
        WAVEHeader {
            waveNumChannels = 1,
            waveFrameRate = frameRate,
            waveBitsPerSample = 16,
            waveFrames = Nothing
        }
genWAVE _ = error "Should only call genWAVE on Files"

-- Line Expressions
genSamples :: Int -> Core -> Either Text WAVESamples
genSamples bpm = go
  where
    go :: Core -> Either Text WAVESamples
    go (CoreNote note) =
        pure $ noteToSamples bpm note
    go (CoreApp lineFun lineArgs) =
        applyLineFun lineFun lineArgs
    go _ = error "Should not be called on CoreSongs"

    applyLineFun :: LineFun -> [Core] -> Either Text WAVESamples
    applyLineFun Seq =
        fmap mconcat . traverse go
    applyLineFun Merge =
        fmap mergeSamples . traverse go
    applyLineFun (Repeat n) =
        fmap (mconcat . replicate n . mconcat) . traverse go

applyModifier :: WAVESamples -> NoteModifier -> WAVESamples
applyModifier samples Detached = chopped ++ remainingSilence
  where
    -- Make the last 20% of the note silent
    splitPoint = div (4 * length samples) 5
    (chopped, remaining) = splitAt splitPoint samples
    remainingSilence = [0] <$ remaining
applyModifier samples Staccato = chopped ++ remainingSilence
  where
    -- Make the last 75% of the note silent
    splitPoint = div (length samples) 4
    (chopped, remaining) = splitAt splitPoint samples
    remainingSilence = [0] <$ remaining

noteToSamples :: Int -> Note -> WAVESamples
noteToSamples bpm (Note noteMods beats pitch) =
    let secondsPerBeat = (beats / toRational bpm) * 60
        durationFrames = secondsPerBeat * toRational frameRate
        unmodifiedSamples =
            take (round durationFrames)
            . mconcat
            . repeat
            $ pitchWave pitch
     in foldl' applyModifier unmodifiedSamples noteMods

-- Sample line constituting a single wavelength of the pitch.
-- frameRate / frequency = wavelength in frames
pitchWave :: Pitch -> WAVESamples
pitchWave Rest = [[0]]
pitchWave (Sound letter accidental octave) =
     mconcat
         [ replicate halfWaveFrames [amplitude]
         , replicate halfWaveFrames [-amplitude]
         ]
  where
    -- Frequency of `Sound letter Nothing 4`
    -- Obtained from https://en.wikipedia.org/wiki/Piano_key_frequencies
    -- We assume most notes will be close to octave 4 to optimize
    -- frequency calculation below
    baseFrequency :: Rational
    baseFrequency =
        case letter of
            C -> 261.6256
            D -> 293.6648
            E -> 329.6276
            F -> 349.2282
            G -> 391.9954
            A -> 440.0000
            B -> 493.8833

    accidentalMultiplier :: Rational
    accidentalMultiplier =
        case accidental of
            Nothing    -> 1
            Just Flat  -> 1 / semitoneFreqMultiplier
            Just Sharp -> semitoneFreqMultiplier

    -- Note: `octave` should be valid (0 <= octave <= 8) from parsing
    frequency :: Rational
    frequency =
        accidentalMultiplier * baseFrequency * (2^^(octave - 4))

    halfWaveFrames :: Int
    halfWaveFrames =
        fromEnum $
            (toRational frameRate / frequency) / 2

