{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.Parser where

import           Numeric

import           Data.Char                  (digitToInt)
import           Data.List
import           Data.List.NonEmpty         as NE
import qualified Data.Set                   as Set
import           Data.Void

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Octune.AST

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
openSong = lexeme (char '{') $> ()

closeSong :: Parser ()
closeSong = lexeme (char '}') $> ()

openSeq :: Parser ()
openSeq = lexeme (char '[') $> ()

closeSeq :: Parser ()
closeSeq = lexeme (char ']') $> ()

openRepeat :: Parser ()
openRepeat = lexeme (string "[*") $> ()

closeRepeat :: Parser ()
closeRepeat = lexeme (string "*]") $> ()

openMerge :: Parser ()
openMerge = lexeme (string "[+") $> ()

closeMerge :: Parser ()
closeMerge = lexeme (string "+]") $> ()

openLine :: Parser ()
openLine = lexeme (char '<') $> ()

closeLine :: Parser ()
closeLine = lexeme (char '>') $> ()


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
        let expected = fmap (Tokens . NE.fromList . show) [0..8]
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
    char '\''
    mStac <- optional (char '\'')
    case mStac of
        Nothing -> pure Detached
        Just _  -> pure Staccato

pBeats :: Parser Beats
pBeats = pRational
  where
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
pLineExpr = try pVar <|> try pLine <|> pLineApp

pVar :: Parser AST
pVar = Var <$> pIdentifier

pLine :: Parser AST
pLine = between openLine closeLine $
    Line <$> many pNote

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
