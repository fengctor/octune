module Octune.Parser.AST where

import           Data.Text            (Text)
import qualified Data.Text            as T

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Data.Combinator
import           Octune.Parser.Lexeme
import           Octune.Parser.Note
import           Octune.Types

initAnn :: SourcePos -> Ann
initAnn srcPos =
    Ann
        { pos = srcPos
        , beatLength = Nothing
        }

getAnn :: Parser Ann
getAnn = initAnn <$> getSourcePos

pModuleComponent :: Parser Text
pModuleComponent = T.pack <$> ((:) <$> upperChar <*> many letterChar)

pModuleDeclaration :: Parser [Text]
pModuleDeclaration = lexeme $
    moduleKW *> pModuleComponent `sepBy1` char '.'

pFile :: Parser (AST Ann)
pFile = lexeme space *> pFileBase
  where
    pFileBase =
        File
        <$> getAnn
        <*> pModuleDeclaration
        <*> some pDecl <* eof

pDecl :: Parser (AST Ann)
pDecl = Decl <$> getAnn <*> (identifier <* equal) <*> pRhs
  where
    pRhs = try pLineExpr <|> pSongExpr

pSongExpr :: Parser (AST Ann)
pSongExpr = between openSong closeSong $
    Song <$> getAnn <*> integer <* colon <*> pLineExpr

pLineExpr :: Parser (AST Ann)
pLineExpr = try pLineNote <|> pVar <|> pLineApp

pQualifiedName :: Parser QualifiedName
pQualifiedName =
    QualName <$> many (pModuleComponent <* char '.') <*> identifier

pVar :: Parser (AST Ann)
pVar = Var <$> getAnn <*> pQualifiedName

pLineNote :: Parser (AST Ann)
pLineNote = LineNote <$> getAnn <*> pNote

pLineApp :: Parser (AST Ann)
pLineApp = pRepeatApp <|> pMergeApp <|> pSeqApp
  where
    pRepeatApp = between openRepeat closeRepeat $
        LineApp
        <$> getAnn
        <*> (Repeat <$> (integer <* colon))
        <*> some (pBeatAssert <|> pLineExpr)
    pMergeApp = between openMerge closeMerge $
        LineApp
        <$> getAnn
        <^> Merge
        <*> some pLineExpr
    pSeqApp = between openSeq closeSeq $
        LineApp
        <$> getAnn
        <^> Seq
        <*> some (pBeatAssert <|> pLineExpr)

pBeatAssert :: Parser (AST Ann)
pBeatAssert = lexeme $
    BeatsAssertion
    <$> getAnn
    <*> (char '|' *> optional (pBeats <* char '>'))
