module Octune.Types.AST where

import           Data.Text         (Text)

import           Control.Lens

import           Data.Combinator

import           Octune.Types.Note

data Waveform
    = Square
    | Sawtooth
    | Triangle
    deriving (Show, Read, Eq, Ord)

data LineFun
    -- Sequence of samples
    = Seq
    -- Layering samples
    | Merge
    -- Repeating sequence of samples
    | Repeat !Int
    -- Set waveform to use
    | UsingWaveform Waveform
    -- Set amplitude of generated samples
    | Volume Rational
    -- Take the subsection of a sequence of samples between
    --   the ends of the given beats
    -- Note: the end of the 0th beat is the beginning of the 1st beat
    | Subsection Beats Beats
    deriving (Show, Read, Eq)

data QualifiedName
    = QualName
        { moduleQual   :: [Text]
        , variableName :: Text
        }
    deriving (Show, Read, Eq)

instance Ord QualifiedName where
    compare (QualName m1 v1) (QualName m2 v2) =
        compare (v1, m1) (v2, m2)

data AST a
    -- Decls in a file
    = File a [Text] [AST a]
    | Decl a Text (AST a)
    -- Song expression: (BPM, LineExpr)
    | Song a !Int (AST a)
    -- Line expressions
    | Var a QualifiedName
    | LineNote a Note
    | LineApp a !LineFun [AST a]
    -- Compile-time check indicators
    | BeatsAssertion a (Maybe Beats)
    deriving (Show, Read, Eq)

annotation :: Lens' (AST a) a
annotation handler (File ann m ds) =
    File <$> handler ann <^> m <^> ds
annotation handler (Decl ann v expr) =
    Decl <$> handler ann <^> v <^> expr
annotation handler (Song ann bpm expr) =
    Song <$> handler ann <^> bpm <^> expr
annotation handler (Var ann v) =
    Var <$> handler ann <^> v
annotation handler (LineNote ann note) =
    LineNote <$> handler ann <^> note
annotation handler (LineApp ann lFun args) =
    LineApp <$> handler ann <^> lFun <^> args
annotation handler (BeatsAssertion ann mBeats) =
    BeatsAssertion <$> handler ann <^> mBeats


-- Prisms for `AST a`

_File :: Prism' (AST a) (a, [Text], [AST a])
_File = prism' embed match
  where
    embed (ann, m, ds) = File ann m ds
    match (File ann m ds) = Just (ann, m, ds)
    match _               = Nothing

_Decl :: Prism' (AST a) (a, Text, AST a)
_Decl = prism' embed match
  where
    embed (ann, v, expr) = Decl ann v expr
    match (Decl ann v expr) = Just (ann, v, expr)
    match _                 = Nothing

_Song :: Prism' (AST a) (a, Int, AST a)
_Song = prism' embed match
  where
    embed (ann, bpm, expr) = Song ann bpm expr
    match (Song ann bpm expr) = Just (ann, bpm, expr)
    match _                   = Nothing

_Var :: Prism' (AST a) (a, QualifiedName)
_Var = prism' embed match
  where
    embed (ann, v) = Var ann v
    match (Var ann v) = Just (ann, v)
    match _           = Nothing

_LineNote :: Prism' (AST a) (a, Note)
_LineNote = prism' embed match
  where
    embed (ann, note) = LineNote ann note
    match (LineNote ann note) = Just (ann, note)
    match _                   = Nothing

_LineApp :: Prism' (AST a) (a, LineFun, [AST a])
_LineApp = prism' embed match
  where
    embed (ann, lFun, args) = LineApp ann lFun args
    match (LineApp ann lFun args) = Just (ann, lFun, args)
    match _                       = Nothing

_BeatsAssertion :: Prism' (AST a) (a, Maybe Beats)
_BeatsAssertion = prism' embed match
  where
    embed (ann, mBeats) = BeatsAssertion ann mBeats
    match (BeatsAssertion ann mBeats) = Just (ann, mBeats)
    match _                           = Nothing
