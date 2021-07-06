module Octune.CodeGen.SamplesGen where

import           GHC.Real        (Ratio (..))

import           Control.Lens
import           Control.Monad

import           Data.Bits
import           Data.Int
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

import           Data.WAVE       (WAVESamples)

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

-- Layer a list of samples over each other
mergeSamples :: [WAVESamples] -> WAVESamples
mergeSamples = foldl1' (zipWithHom (zipWithHom (+)))

genSamples :: Env Core -> Int -> Int -> Core -> WAVESamples
genSamples env bpm frameRate = memoGenSamples
  where
    memoGenSamples :: Core -> WAVESamples
    memoGenSamples (CoreVar qName) = cache Map.! qName
    memoGenSamples coreExpr        = go coreExpr

    -- Note: Strict Map is ok here since getting WHNF of WAVESamples
    --       will not evaluate the spine of the list
    cache :: Map QualifiedName WAVESamples
    cache = fmap go env

    go :: Core -> WAVESamples
    go (CoreVar vName)            = memoGenSamples (env Map.! vName)
    go (CoreNote note)            = noteToSamples bpm frameRate note
    go (CoreApp lineFun lineArgs) = applyLineFun lineFun lineArgs
    go _                          = error "Should not be called on CoreSongs"

    multRat :: Rational -> Int32 -> Int32
    multRat (num :% denom) =
        (`div` fromIntegral denom)
        . (* fromIntegral num)

    applyLineFun :: LineFun -> [Core] -> WAVESamples
    applyLineFun Seq          = (memoGenSamples =<<)
    applyLineFun Merge        = mergeSamples . fmap memoGenSamples
    applyLineFun (Repeat n)   = join . replicate n . (memoGenSamples =<<)
    applyLineFun (Volume rat) = (fmap . fmap) (multRat rat) . (memoGenSamples =<<)

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
noteToSamples :: Int -> Int -> Note -> WAVESamples
noteToSamples bpm frameRate (Note noteMods beats sound) =
    foldlOf' traversed (flip applyModifier) unmodifiedSamples noteMods
  where
    secondsPerBeat = (beats / toRational bpm) * 60
    durationFrames = round (secondsPerBeat * toRational frameRate)
    unmodifiedSamples = take durationFrames $ cycle (soundWave frameRate sound)

-- Sample line constituting a single wavelength of the sound.
-- frameRate / frequency = wavelength in frames
soundWave :: Int -> Sound -> WAVESamples
soundWave _ Rest = [[0]]
-- TODO: adjust based on framerate
soundWave _ (Drum percussion) =
    case percussion of
        Snare -> snareSample ++ repeat [0]
        Clap  -> clapSample ++ repeat [0]
soundWave frameRate (Pitch letter accidental octave) = squareWave
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

    squareWave :: WAVESamples
    squareWave =
        let wavelenFrames = round (toRational frameRate / frequency)
            firstHalf = wavelenFrames `div` 2
            secondHalf = wavelenFrames - firstHalf
         in mconcat
                [ replicate firstHalf [-amplitude]
                , replicate firstHalf [amplitude]
                , replicate secondHalf [-amplitude]
                , replicate secondHalf [amplitude]
                ]
