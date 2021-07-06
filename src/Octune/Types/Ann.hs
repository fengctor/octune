module Octune.Types.Ann where

import           Control.Lens

import           Text.Megaparsec

import           Octune.Types.Note (Beats)

data Ann
    = Ann
        { _pos        :: !SourcePos
        , _beatLength :: Maybe Beats
        }
    deriving (Show, Read, Eq)

pos :: Lens' Ann SourcePos
pos handler ann =
    (\p -> ann { _pos = p }) <$> handler (_pos ann)

beatLength :: Lens' Ann (Maybe Beats)
beatLength handler ann =
    (\b -> ann { _beatLength = b }) <$> handler (_beatLength ann)
