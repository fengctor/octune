{-# LANGUAGE BangPatterns #-}

module Octune.Types.Note where

data Letter
    = C | D | E | F | G | A | B
    deriving (Show, Read, Eq)

data Accidental
    = Flat
    | Sharp
    deriving (Show, Read, Eq)

type Octave = Int

data Pitch
    = Sound !Letter !(Maybe Accidental) !Octave
    | Rest
    deriving (Show, Read, Eq)

-- TODO: more modifiers
data NoteModifier
    = Detached
    | Staccato
    deriving (Show, Read, Eq)

type Beats = Rational

data Note = Note [NoteModifier] !Beats Pitch
    deriving (Show, Read, Eq)


