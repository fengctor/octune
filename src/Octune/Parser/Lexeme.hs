{-# LANGUAGE ExtendedDefaultRules #-}
{-# LANGUAGE OverloadedStrings    #-}

module Octune.Parser.Lexeme where

import           Data.Void

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void Text

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

openUsingWaveform :: Parser ()
openUsingWaveform = () <$ lexeme (string "[^")

closeUsingWaveform :: Parser ()
closeUsingWaveform = () <$ lexeme (string "^]")

openVolume :: Parser ()
openVolume = () <$ lexeme (string "[!")

closeVolume :: Parser ()
closeVolume = () <$ lexeme (string "!]")

moduleKW :: Parser Text
moduleKW = lexeme (string "module")

identifier :: Parser Text
identifier = T.pack <$>
    lexeme ((:) <$> lowerChar <*> many idChar <?> "identifier")
  where
    idChar :: Parser Char
    idChar = alphaNumChar <|> char '#'

integer :: Parser Int
integer = lexeme L.decimal

equal :: Parser ()
equal = () <$ lexeme (char '=')

colon :: Parser ()
colon = () <$ lexeme (char ':')
