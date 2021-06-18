{-# LANGUAGE BangPatterns #-}

module Octune.Types.Ann where

import           Octune.Types.Note (Beats)
import           Text.Megaparsec

data Ann
    = Ann
        { pos     :: !SourcePos
        , beatLen :: Maybe Beats
        }
    deriving (Show, Read, Eq)
