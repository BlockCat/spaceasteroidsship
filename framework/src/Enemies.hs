{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Enemies where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Player

data Enemy = Enemy {
        enemyLocation  :: Point,        
        enemyDirection :: Float,
        enemySpeed     :: Vector,
        enemyTimer     :: Float,
        updateEnemy    :: Enemy -> Player -> Float -> Enemy
    }

drawEnemies :: [Enemy] -> Picture -> Picture
drawEnemies xs pict = Pictures $ map (drawEnemy pict) xs

drawEnemy :: Picture -> Enemy -> Picture
drawEnemy pict Enemy{..} = translate x y (color red triangle)
    where 
        triangle = rotate (radToDeg enemyDirection + 270) pict
        (x, y)   = enemyLocation
              
              
spawnEnemyAtRandomLocation :: StdGen -> (Enemy, StdGen)
spawnEnemyAtRandomLocation stdGen = (Enemy {enemyLocation = (x', y'), enemyDirection = 0, enemySpeed = (0, 0), updateEnemy = simpleBehaviour, enemyTimer = 4}, r2)
    where
        (x', r1) = randomR (-1000, 1000) stdGen 
        (y', r2) = randomR (-1000, 1000) r1
        

            

simpleBehaviour :: Enemy -> Player -> Float -> Enemy
simpleBehaviour enemy@(Enemy{..}) player@(Player{..}) time = enemy {enemySpeed = newEnemySpeed, enemyDirection = newDirection}
        where
            enemyVelocity  = 150 * time
            vectorToPlayer = playerLocation - enemyLocation
            newEnemySpeed  = (mulSV enemyVelocity . normalizeV) vectorToPlayer
            newDirection   = directionToPlayer enemy player
            
jumperBehaviour :: Enemy -> Player -> Float -> Enemy
jumperBehaviour enemy@(Enemy{..}) player@(Player{..}) time = enemy {enemySpeed = speedVec, enemyDirection = newDirection, enemyTimer = enemyTimer'}
        where
            enemyTimer'    = if enemyTimer < 0 then 4 else enemyTimer - time
            enemyVelocity  = enemyTimer * 200 * time            
            newDirection   = if enemyTimer' > 0 then enemyDirection else directionToPlayer enemy player
            speedVec       = (mulSV enemyVelocity . unitVectorAtAngle) newDirection
            
moveEnemies:: [Enemy] -> [Enemy]
moveEnemies xs = [x {enemyLocation = enemyLocation x + (enemySpeed x)} | x <- enemies]
        where
            enemies = separation xs

separation :: [Enemy] -> [Enemy]
separation xs = [separate x xs | x <- xs]

            
separate :: Enemy -> [Enemy] -> Enemy
separate enemy xs = enemy {enemySpeed = enemySpeed enemy + (mulSV (1/(fromIntegral (length xs))) . sum . distances enemy) xs}
--        where
            
distances enemy = filter (\a -> a /=(0,0) && magV a <= 30) . map (\e -> (enemyLocation enemy) - (enemyLocation e))
                

directionToPlayer :: Enemy -> Player -> Float
directionToPlayer enemy@(Enemy{..}) player@(Player{..}) = direction
        where
            direction = atan2 y x
            (x, y)    = playerLocation - enemyLocation
