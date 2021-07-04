module Octune.Types.Note where

data Letter
    = C | D | E | F | G | A | B
    deriving (Show, Read, Eq)

data Accidental
    = Flat
    | Sharp
    deriving (Show, Read, Eq)

type Octave = Int

data Percussion
    = Snare
    | Clap
    deriving (Show, Read, Eq)

data Sound
    = Pitch !Letter !(Maybe Accidental) !Octave
    | Drum Percussion
    | Rest
    deriving (Show, Read, Eq)

-- TODO: more modifiers
data NoteModifier
    = Detached
    | Staccato
    deriving (Show, Read, Eq)

type Beats = Rational

data Note = Note [NoteModifier] !Beats Sound
    deriving (Show, Read, Eq)


