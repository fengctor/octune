module Octune.Types.AST where

import           Data.Text         (Text)

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

getAug :: AST a -> a
getAug (File a _ _)         = a
getAug (Decl a _ _)         = a
getAug (Song a _ _)         = a
getAug (Var a _)            = a
getAug (LineNote a _)       = a
getAug (LineApp a _ _)      = a
getAug (BeatsAssertion a _) = a
