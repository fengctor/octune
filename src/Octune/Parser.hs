{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.Parser where

import           Numeric

import           Data.Char                  (digitToInt)
import           Data.List
import           Data.Void

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import           Octune.AST

type Parser = Parsec Void Text

lexeme :: Parser a -> Parser a
lexeme =
    L.lexeme $
        L.space
            space1
            (L.skipLineComment "--")
            (L.skipBlockComment "{-" "-}")

openSong :: Parser ()
openSong = lexeme (char '{') *> pure ()

closeSong :: Parser ()
closeSong = lexeme (char '}') *> pure ()

openSeq :: Parser ()
openSeq = lexeme (char '[') *> pure ()

closeSeq :: Parser ()
closeSeq = lexeme (char ']') *> pure ()

openRepeat :: Parser ()
openRepeat = lexeme (string "[*") *> pure ()

closeRepeat :: Parser ()
closeRepeat = lexeme (string "*]") *> pure ()

openMerge :: Parser ()
openMerge = lexeme (string "[+") *> pure ()

closeMerge :: Parser ()
closeMerge = lexeme (string "+]") *> pure ()

openLine :: Parser ()
openLine = lexeme (char '<') *> pure ()

closeLine :: Parser ()
closeLine = lexeme (char '>') *> pure ()

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

pPitch :: Parser Pitch
pPitch =
    Rest <$ char '_'
    <|>
    Sound <$> pLetter <*> optional (try pAccidental) <*> L.decimal

mantissaToRational :: String -> Rational
mantissaToRational = go (1 / 10)
  where
    go :: Rational -> String -> Rational
    go _ [] = 0
    go colMult (d:ds) =
        colMult * toRational (digitToInt d) + go (colMult / 10) ds

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
pNote = lexeme $ Note <$> pBeats <*> pPitch

pFile :: Parser AST
pFile = File <$> (lexeme space *> some pDecl <* eof)

pIdentifier :: Parser Text
pIdentifier = T.pack <$>
    lexeme ((:) <$> letterChar <*> many alphaNumChar <?> "identifier")

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
