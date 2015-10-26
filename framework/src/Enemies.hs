{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Enemies where

import Graphics.Gloss

data Enemy = Enemy {
        enemyLocation  :: Point,
        enemySpeed     :: Vector,
        enemyDirection :: Float
    }

drawEnemy :: Enemy -> Picture
drawEnemy Enemy{..}    = translate x y (color red triangle)
        where triangle = rotate (90-enemyDirection :: Float)  $ polygon [(-5, -5), (5, -5), (0, 5)]
              (x, y)   = enemyLocation