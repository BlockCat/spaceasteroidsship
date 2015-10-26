{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Enemies where

import Graphics.Gloss
import System.Random

data Enemy = Enemy {
        enemyLocation  :: Point,
        enemySpeed     :: Vector,
        enemyDirection :: Float
    }

drawEnemies :: [Enemy] -> Picture
drawEnemies xs = Pictures $ map drawEnemy xs

drawEnemy :: Enemy -> Picture
drawEnemy Enemy{..}    = translate x y (color red triangle)
        where triangle = rotate (90-enemyDirection :: Float)  $ polygon [(-5, -5), (5, -5), (0, 5)]
              (x, y)   = enemyLocation
              
              
spawnEnemyAtRandomLocation :: StdGen -> (Enemy, StdGen)
spawnEnemyAtRandomLocation stdGen = (Enemy {enemyLocation = (x', y'), enemySpeed = (0, 0), enemyDirection = 0}, r2)
        where
            (x', r1) = randomR ((-1000), 1000) stdGen 
            (y', r2) = randomR ((-1000), 1000) r1