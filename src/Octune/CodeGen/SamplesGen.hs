module Octune.CodeGen.SamplesGen where

import           Control.Lens
import           Control.Monad
import           Control.Monad.Par

import           Data.Bits
import           Data.Int
import           Data.List
import qualified Data.Map.Strict   as Map
import           Data.Maybe

import           Data.WAVE         (WAVESamples)

import           Data.Sounds

import           Octune.Types

-- Default amplitude of a wave
amplitude :: Int32
amplitude = 1 `shiftL` 27 + 1 `shiftL` 26

-- Multiplier for frequency to go up a semitone
semitoneFreqMultiplier :: Rational
semitoneFreqMultiplier = 1.05946309435929


-- {-# INLINE [0] zipWithHom #-}
zipWithHom :: (a -> a -> a) -> [a] -> [a] -> [a]
zipWithHom f = go
  where
    go [] ys         = ys
    go xs []         = xs
    go (x:xs) (y:ys) = f x y : go xs ys

-- Combine a list of samples one after another
sequenceSamples :: [WAVESamples] -> WAVESamples
sequenceSamples = join

-- Layer a list of samples over each other
mergeSamples :: [WAVESamples] -> WAVESamples
mergeSamples = foldl1' (zipWithHom (zipWithHom (+)))

-- Sequence a list of samples,
--   then repeat it a given number of times
repeatSamples :: Int -> [WAVESamples] -> WAVESamples
repeatSamples n = sequenceSamples . replicate n . sequenceSamples

modifySamplesVolume :: Rational -> WAVESamples -> WAVESamples
modifySamplesVolume multiplier = (fmap . fmap) (multRat multiplier)
  where
    multRat :: Rational -> Int32 -> Int32
    multRat rat = round . (* rat) . toRational

waveformOrDefault :: Maybe Waveform -> Waveform
waveformOrDefault = fromMaybe Square

-- TODO: Add option to turn memoization on.
-- By the nature of Octune code, very few instances
--   will benefit from memoizing samples for variables.
-- Many times, variables will not be mentioned more
--   than a couple of times, in which case the overhead
--   of memoization is not worth it.
-- When variables are "repeated", it is usually through
--   a `Repeat` block ([* n : vars... *]), in which case
--   the samples for each variable is still only generated once,
--   with the generated samples themselves being replicated
genSamples :: Env Core -> Int -> Int -> Bool -> Core -> WAVESamples
genSamples env bpm frameRate _memoize = runPar . go Nothing
  where
    {-
    memoGenSamples :: Maybe Waveform -> Core -> WAVESamples
    memoGenSamples mWaveform coreExpr
      | CoreVar qName <- coreExpr, memoize =
          cache Map.! waveformOrDefault mWaveform Map.! qName
      | otherwise = go mWaveform coreExpr

    -- Note: Strict Map is ok here since getting WHNF of WAVESamples
    --       will not evaluate the spine of the list
    cache :: Map Waveform (Map QualifiedName WAVESamples)
    cache = Map.fromList
        [ (Square, fmap (go $ Just Square) env)
        , (Sawtooth, fmap (go $ Just Sawtooth) env)
        ]
    -}
    go :: Maybe Waveform -> Core -> Par WAVESamples
    go mWaveform (CoreVar vName) =
        go mWaveform (env Map.! vName)
    go mWaveform (CoreNote note) =
        pure $ noteToSamples bpm frameRate note mWaveform
    go mWaveform (CoreApp lineFun lineArgs) =
        applyLineFun mWaveform lineFun lineArgs
    go _ _ = error "Should not be called on CoreSongs"

    applyLineFun :: Maybe Waveform -> LineFun -> [Core] -> Par WAVESamples
    applyLineFun mWaveform Seq =
        fmap sequenceSamples
        . parMapM (go mWaveform)
    applyLineFun mWaveform Merge =
        fmap mergeSamples
        . parMapM (go mWaveform)
    applyLineFun mWaveform (Repeat n) =
        fmap (repeatSamples n)
        . parMapM (go mWaveform)
    applyLineFun mWaveform (UsingWaveform setWaveform) =
        fmap sequenceSamples
        . parMapM (go nextMWaveform)
      where
        -- Sets the new specified waveform if one has not been set
        --   in an outer UsingWaveform block
        nextMWaveform = Just $ fromMaybe setWaveform mWaveform
    applyLineFun mWaveform (Volume rat) =
        fmap (modifySamplesVolume rat . sequenceSamples)
        . parMapM (go mWaveform)

applyModifier :: NoteModifier -> WAVESamples -> WAVESamples
applyModifier Detached samples =
    -- Make the last 20% of the note silent
    samples & dropping keptSamples traversed . traversed .~ 0
  where
    keptSamples = div (4 * length samples) 5
applyModifier Staccato samples =
    -- Make the last 75% of the note silent
    samples & dropping keptSamples traversed . traversed .~ 0
  where
    keptSamples = div (length samples) 4

-- TODO: figure out how to use Folds to get `unmodifiedSamples`
--       without sacrificing performance
noteToSamples :: Int -> Int -> Note -> Maybe Waveform -> WAVESamples
noteToSamples bpm frameRate (Note noteMods beats sound) mWaveform =
    foldlOf' traversed (flip applyModifier) unmodifiedSamples noteMods
  where
    secondsPerBeat = (beats / toRational bpm) * 60
    durationFrames = round (secondsPerBeat * toRational frameRate)
    unmodifiedSamples =
        take durationFrames . cycle $
            soundWave frameRate sound mWaveform

-- Sample line constituting a single wavelength of the sound.
-- frameRate / frequency = wavelength in frames
soundWave :: Int -> Sound -> Maybe Waveform -> WAVESamples
soundWave _ Rest _ = [[0]]
-- TODO: adjust based on framerate
soundWave _ (Drum percussion) _ =
    case percussion of
        Snare -> snareSample ++ repeat [0]
        Clap  -> clapSample ++ repeat [0]
soundWave frameRate (Pitch letter accidental octave) mWaveform =
    case waveformOrDefault mWaveform of
        Square ->
            mconcat
                [ replicate firstHalf [-amplitude]
                , replicate firstHalf [amplitude]
                , replicate secondHalf [-amplitude]
                , replicate secondHalf [amplitude]
                ]
              where
                firstHalf = wavelenFrames `div` 2
                secondHalf = wavelenFrames - firstHalf
        Sawtooth ->
            -- `2*amplitude` max amplitude to reach the same energy as
            -- a square wave with amplitude `amplitude`
            fmap (pure . fromIntegral . lineEquation) [0..wavelenFrames-1]
              where
                intAmplitude = fromIntegral amplitude
                -- slope is (2*amplitude - 2*(-amplitude)) / wavelenFrames
                lineEquation i =
                    ((4*intAmplitude*i) `div` wavelenFrames)
                    -
                    (2*intAmplitude)
  where
    -- Frequency of `Sound letter accidental 4`
    -- Obtained from https://en.wikipedia.org/wiki/Piano_key_frequencies
    -- We assume most notes will be close to octave 4 to optimize
    -- frequency calculation below
    baseFrequency :: Rational
    baseFrequency =
        case (letter, accidental) of
            (C, Just Flat)  -> 246.9417
            (C, Nothing)    -> 261.6256
            (C, Just Sharp) -> 277.1826
            (D, Just Flat)  -> 277.1826
            (D, Nothing)    -> 293.6648
            (D, Just Sharp) -> 311.1270
            (E, Just Flat)  -> 311.1270
            (E, Nothing)    -> 329.6276
            (E, Just Sharp) -> 349.2282
            (F, Just Flat)  -> 329.6276
            (F, Nothing)    -> 349.2282
            (F, Just Sharp) -> 369.9944
            (G, Just Flat)  -> 369.9944
            (G, Nothing)    -> 391.9954
            (G, Just Sharp) -> 415.3047
            (A, Just Flat)  -> 415.3047
            (A, Nothing)    -> 440.0000
            (A, Just Sharp) -> 466.1638
            (B, Just Flat)  -> 466.1638
            (B, Nothing)    -> 493.8833
            (B, Just Sharp) -> 523.2511

    -- Note: `octave` should be valid (0 <= octave <= 8) from parsing
    frequency :: Rational
    frequency = baseFrequency * (2^^(octave - 4))

    wavelenFrames :: Int
    wavelenFrames = round (toRational frameRate / frequency)
