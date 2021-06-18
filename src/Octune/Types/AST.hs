{-# LANGUAGE BangPatterns #-}

module Octune.Types.AST where

import           Data.Text         (Text)

import           Octune.Types.Note

data LineFun
    = Seq
    | Merge
    | Repeat !Int
    deriving (Show, Read, Eq)

data AST a
    -- Decls in a file
    = File a [AST a]
    | Decl a Text (AST a)
    -- Song expression: (BPM, LineExpr)
    | Song a !Int (AST a)
    -- Line expressions
    | Var a Text
    | LineNote a Note
    | LineApp a !LineFun [AST a]
    -- Compile-time check indicators
    | BeatsAssertion a (Maybe Beats)
    deriving (Show, Read, Eq)

getAug :: AST a -> a
getAug (File a _)           = a
getAug (Decl a _ _)         = a
getAug (Song a _ _)         = a
getAug (Var a _)            = a
getAug (LineNote a _)       = a
getAug (LineApp a _ _)      = a
getAug (BeatsAssertion a _) = a
