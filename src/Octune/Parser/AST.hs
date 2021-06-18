module Octune.Parser.AST where

import           Text.Megaparsec
import           Text.Megaparsec.Char

import           Octune.Parser.Lexeme
import           Octune.Parser.Note
import           Octune.Types

pFile :: Parser AST
pFile = File <$> (lexeme space *> some pDecl <* eof)

pDecl :: Parser AST
pDecl = Decl <$> identifier <* equal <*> pRhs
  where
    pRhs = try pLineExpr <|> pSongExpr

pSongExpr :: Parser AST
pSongExpr = between openSong closeSong $
    Song <$> integer <* colon <*> pLineExpr

pLineExpr :: Parser AST
pLineExpr = try pLineNote <|> pVar <|> pLineApp

pVar :: Parser AST
pVar = Var <$> identifier

pLineNote :: Parser AST
pLineNote = LineNote <$> pNote

pLineApp :: Parser AST
pLineApp =
    pRepeatApp <|> pMergeApp <|> pSeqApp
  where
    pRepeatApp = between openRepeat closeRepeat $
        LineApp
        <$> (Repeat <$> integer <* colon)
        <*> some pLineExpr
    pMergeApp = between openMerge closeMerge $
        LineApp Merge <$> some pLineExpr
    pSeqApp = between openSeq closeSeq $
        LineApp Seq <$> some pLineExpr
