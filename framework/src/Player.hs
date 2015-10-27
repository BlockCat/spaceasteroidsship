{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Player where

import Graphics.Gloss (Vector, Point, Picture)


data Player = Player {
        --Player location
        playerLocation  :: Point,
        playerSpeed     :: Vector,
        direction       :: Float,
        playerImage     :: Picture
    }