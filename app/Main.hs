module Main where

--- Example from https://github.com/BartMassey/wave/blob/master/writetest.hs
--- Write a 1-second 48Ksps 240Hz half-amplitude mono square
--- wave into "square.wav".

import           Data.Bits
import           Data.List

import           Data.WAVE

mergeSamples :: [WAVESamples] -> WAVESamples
mergeSamples = foldl1' (zipWith (zipWith (+)))

main :: IO ()
main = do
  let header =
          WAVEHeader {
             waveNumChannels = 1,
             waveFrameRate = 48000,
             waveBitsPerSample = 16,
             waveFrames = Nothing
          }
  let ampl = 1 `shiftL` 28
  let sample n = concat $ replicate (240 * n) $
       replicate (div 100 n) [-ampl] ++ replicate (div 100 n) [ampl]
  let squareFirst =
          WAVE {
             waveHeader = header,
             waveSamples = sample 1
          }
  let squareSecond =
          WAVE {
             waveHeader = header,
             waveSamples = sample 2
          }
  let squareBoth =
          WAVE {
             waveHeader = header,
             waveSamples = mergeSamples [sample 1, sample 2, sample 3]
          }
  putWAVEFile "first.wav" squareFirst
  putWAVEFile "second.wav" squareSecond
  putWAVEFile "both.wav" squareBoth
  putStrLn "Done"
