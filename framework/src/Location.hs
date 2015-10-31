{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Location where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point 
import Graphics.Gloss

import RandomUtils

class Location a where {
    location  :: a -> Point    
}

