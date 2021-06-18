{-# LANGUAGE BangPatterns #-}

module Octune.Types.AST where

import           Data.Text         (Text)

import           Octune.Types.Note

data LineFun
    = Seq
    | Merge
    | Repeat !Int
    deriving (Show, Read, Eq)

data AST
    -- Decls in a file
    = File [AST]
    | Decl Text AST
    -- Song expression
    | Song !Int AST          -- BPM, Line
    -- Line expressions
    | Var Text
    | LineNote Note           -- Row of notes
    | LineApp !LineFun [AST] -- Function application on lines
    deriving (Show, Read, Eq)
