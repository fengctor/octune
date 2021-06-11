module Octune.AST where

import           Data.Text (Text)
import qualified Data.Text as T

data Letter
    = C | D | E | F | G | A | B
    deriving (Show, Read, Eq)

data Accidental
    = Flat
    | Sharp
    deriving (Show, Read, Eq)

type Octave = Int
data Pitch
    = Sound Letter (Maybe Accidental) Octave
    | Rest
    deriving (Show, Read, Eq)

type Beats = Rational

data Note = Note Pitch Beats
    deriving (Show, Read, Eq)

data LineFun
    = Seq
    | Merge
    | Repeat Int
    deriving (Show, Read, Eq)

data AST
    -- Decls in a file
    = File [AST]
    | Decl Text AST
    -- Song expression
    | Song Int AST          -- BPM, Line
    -- Line expressions
    | Var Text
    | Line [Note]           -- Row of notes
    | LineApp LineFun [AST] -- Function application on lines
    deriving (Show, Read, Eq)
