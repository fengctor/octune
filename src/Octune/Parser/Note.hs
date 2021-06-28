{-# LANGUAGE TypeApplications #-}

module Octune.Parser.Note where

import           Data.Char                  (digitToInt)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as Set

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Octune.Parser.Lexeme
import           Octune.Types


pLetter :: Parser Letter
pLetter =
    C <$ char 'C'
    <|>
    D <$ char 'D'
    <|>
    E <$ char 'E'
    <|>
    F <$ char 'F'
    <|>
    G <$ char 'G'
    <|>
    A <$ char 'A'
    <|>
    B <$ char 'B'

pAccidental :: Parser Accidental
pAccidental =
    Flat <$ char 'b'
    <|>
    Sharp <$ char '#'

pOctave :: Parser Octave
pOctave = digitChar >>= validateOctave
  where
    validateOctave :: Char -> Parser Octave
    validateOctave '9' =
        let expected = fmap (Tokens . NE.fromList . show @Int) [0..8]
         in failure
                (Just $ Tokens (NE.fromList "9"))
                (Set.fromList expected)
    validateOctave n =
        pure $ digitToInt n

pPercussion :: Parser Percussion
pPercussion = do
    _ <- char '%'
    mClap <- optional (char '%')
    pure $ case mClap of
        Nothing -> Snare
        Just _  -> Clap

pPitch :: Parser Pitch
pPitch =
    Rest <$ char '_'
    <|>
    Drum <$> pPercussion
    <|>
    Tone <$> pLetter <*> optional (try pAccidental) <*> pOctave

pNoteModifier :: Parser NoteModifier
pNoteModifier = do
    _ <- char '\''
    mStac <- optional (char '\'')
    pure $ case mStac of
               Nothing -> Detached
               Just _  -> Staccato

mantissaToRational :: String -> Rational
mantissaToRational = go (1 / 10)
  where
    go :: Rational -> String -> Rational
    go _ [] = 0
    go colMult (d:ds) =
        colMult * toRational (digitToInt d) + go (colMult / 10) ds

pRational :: Parser Rational
pRational = do
    base <- L.decimal
    mMantissa <- optional (char '.' *> many digitChar)
    pure $ case mMantissa of
        Nothing       -> base
        Just mantissa -> base + mantissaToRational mantissa

pBeats :: Parser Beats
pBeats = pRelativeBeats <|> pRational
  where
    -- half notes, quarter notes, etc...
    pRelativeBeatsBase :: Parser Beats
    pRelativeBeatsBase =
        2 <$ char 'h'
        <|>
        1 <$ char 'q'
        <|>
        0.5 <$ char 'e'
        <|>
        0.25 <$ char 's'
        <|>
        0.125 <$ char 't'

    -- Considers trailing dots
    pRelativeBeats :: Parser Beats
    pRelativeBeats = do
        base <- pRelativeBeatsBase
        dots <- many (char '.')
        -- Note: 1 + 1/2 + 1/4 + 1/8 + ... + 1/(2^n)
        --     = 2 - (1/2)^n
        pure $ base * (2 - (1/2)^^length dots)

pNote :: Parser Note
pNote = lexeme $ Note <$> many pNoteModifier <*> pBeats <*> pPitch


