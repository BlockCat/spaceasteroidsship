{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Player where

import Graphics.Gloss (Vector, Point, Picture)
import Location

data Player = Player {
        --Player location
        playerLocation  :: Point,
        playerSpeed     :: Vector,
        direction       :: Float,
        playerImage     :: Picture,
        shootTimer      :: Float,
        canShoot        :: Bool
    }
instance Location Player where {
        location = playerLocation
    }