{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Enemies where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Data.Vector
import Player

data Enemy = Enemy {
        enemyLocation  :: Point,        
        enemyDirection :: Float,
        updateEnemy    :: Enemy -> Player -> Float -> Enemy
    }

drawEnemies :: [Enemy] -> Picture
drawEnemies xs = Pictures $ map drawEnemy xs

drawEnemy :: Enemy -> Picture
drawEnemy Enemy{..}    = translate x y (color red triangle)
        where triangle = rotate (pi - enemyDirection)  $ polygon [(-5, -5), (5, -5), (0, 5)]
              (x, y)   = enemyLocation
              
              
spawnEnemyAtRandomLocation :: StdGen -> (Enemy, StdGen)
spawnEnemyAtRandomLocation stdGen = (Enemy {enemyLocation = (x', y'), enemyDirection = 0, updateEnemy = simpleBehaviour}, r2)
        where
            (x', r1) = randomR ((-1000), 1000) stdGen 
            (y', r2) = randomR ((-1000), 1000) r1
            

simpleBehaviour :: Enemy -> Player -> Float -> Enemy
simpleBehaviour enemy@(Enemy{..}) player@(Player{..}) time = enemy {enemyLocation = enemyLocation + vectorToPlayer, enemyDirection = newDirection}
        where
            enemySpeed     = 150 * time
            vectorToPlayer = mulSV enemySpeed $ normalizeV (playerLocation - enemyLocation)
            newDirection   = angleVV vectorToPlayer (1, 0)
            