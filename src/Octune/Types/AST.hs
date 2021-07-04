module Octune.Types.AST where

import           Data.Text         (Text)

import           Control.Lens

import           Data.Combinator

import           Octune.Types.Note

data LineFun
    = Seq
    | Merge
    | Repeat !Int
    | Volume Rational
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
