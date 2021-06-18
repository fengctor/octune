module Octune.Types.Ann where

import           Octune.Types.Note (Beats)
import           Text.Megaparsec

data Ann
    = Ann
        { pos        :: !SourcePos
        , beatLength :: Maybe Beats
        }
    deriving (Show, Read, Eq)
