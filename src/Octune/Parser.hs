{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeApplications     #-}

module Octune.Parser where

import           Data.Char                  (digitToInt)
import qualified Data.List.NonEmpty         as NE
import qualified Data.Set                   as Set
import           Data.Void

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Octune.Types.AST

type Parser = Parsec Void Text

{- Token Definitions -}

lexeme :: Parser a -> Parser a
lexeme =
    L.lexeme $
        L.space
            space1
            (L.skipLineComment "--")
            (L.skipBlockComment "{-" "-}")

openSong :: Parser ()
openSong = () <$ lexeme (char '{')

closeSong :: Parser ()
closeSong = () <$ lexeme (char '}')

openSeq :: Parser ()
openSeq = () <$ lexeme (char '[')

closeSeq :: Parser ()
closeSeq = () <$ lexeme (char ']')

openRepeat :: Parser ()
openRepeat = () <$ lexeme (string "[*")

closeRepeat :: Parser ()
closeRepeat = () <$ lexeme (string "*]")

openMerge :: Parser ()
openMerge = () <$ lexeme (string "[+")

closeMerge :: Parser ()
closeMerge = () <$ lexeme (string "+]")


{- Parser Definitions -}

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

pPitch :: Parser Pitch
pPitch =
    Rest <$ char '_'
    <|>
    Sound <$> pLetter <*> optional (try pAccidental) <*> pOctave

mantissaToRational :: String -> Rational
mantissaToRational = go (1 / 10)
  where
    go :: Rational -> String -> Rational
    go _ [] = 0
    go colMult (d:ds) =
        colMult * toRational (digitToInt d) + go (colMult / 10) ds

pNoteModifier :: Parser NoteModifier
pNoteModifier = do
    _ <- char '\''
    mStac <- optional (char '\'')
    case mStac of
        Nothing -> pure Detached
        Just _  -> pure Staccato

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
        pure $ base * (2 - (1/2)^^(length dots))

    pRational :: Parser Beats
    pRational = do
        base <- L.decimal
        mMantissa <- optional (char '.' *> many digitChar)
        pure $ case mMantissa of
            Nothing       -> base
            Just mantissa -> base + mantissaToRational mantissa

pNote :: Parser Note
pNote = lexeme $ Note <$> many pNoteModifier <*> pBeats <*> pPitch

pFile :: Parser AST
pFile = File <$> (lexeme space *> some pDecl <* eof)

pIdentifier :: Parser Text
pIdentifier = T.pack <$>
    lexeme ((:) <$> letterChar <*> many idChar <?> "identifier")
  where
    idChar :: Parser Char
    idChar = alphaNumChar <|> char '#'

pDecl :: Parser AST
pDecl = Decl <$> pIdentifier <*> (lexeme (char '=') *> pRhs)
  where
    pRhs = try pLineExpr <|> pSongExpr

pSongExpr :: Parser AST
pSongExpr = between openSong closeSong $
    Song <$> lexeme L.decimal <* lexeme (char ':') <*> pLineExpr

pLineExpr :: Parser AST
pLineExpr = try pLineNote <|> pVar <|> pLineApp

pVar :: Parser AST
pVar = Var <$> pIdentifier

pLineNote :: Parser AST
pLineNote = LineNote <$> pNote

pLineApp :: Parser AST
pLineApp =
    pRepeatApp <|> pMergeApp <|> pSeqApp
  where
    pRepeatApp = between openRepeat closeRepeat $
        LineApp
        <$> (Repeat <$> lexeme L.decimal <* lexeme (char ':'))
        <*> some pLineExpr
    pMergeApp = between openMerge closeMerge $
        LineApp Merge <$> some pLineExpr
    pSeqApp = between openSeq closeSeq $
        LineApp Seq <$> some pLineExpr
