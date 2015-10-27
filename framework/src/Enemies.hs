{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Enemies where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Data.Vector
import Player

data Enemy = Enemy {
        enemyLocation  :: Point,        
        enemyDirection :: Float,
        enemyTimer     :: Float,
        updateEnemy    :: Enemy -> Player -> Float -> Enemy
    }

drawEnemies :: [Enemy] -> Picture -> Picture
drawEnemies xs pict = Pictures $ map (drawEnemy pict) xs

drawEnemy :: Picture -> Enemy -> Picture
drawEnemy pict Enemy{..} = translate x y (color red triangle)
        where triangle   = rotate (radToDeg enemyDirection + 270) $ pict
              (x, y)     = enemyLocation
              
              
spawnEnemyAtRandomLocation :: StdGen -> (Enemy, StdGen)
spawnEnemyAtRandomLocation stdGen = (Enemy {enemyLocation = (x', y'), enemyDirection = 0, updateEnemy = simpleBehaviour, enemyTimer = 4}, r2)
        where
            (x', r1) = randomR ((-1000), 1000) stdGen 
            (y', r2) = randomR ((-1000), 1000) r1
            

simpleBehaviour :: Enemy -> Player -> Float -> Enemy
simpleBehaviour enemy@(Enemy{..}) player@(Player{..}) time = enemy {enemyLocation = enemyLocation + vectorToPlayer, enemyDirection = newDirection}
        where
            enemySpeed   = 150 * time
            vectorToPlayer = mulSV enemySpeed $ normalizeV (playerLocation - enemyLocation)
            newDirection = directionToPlayer enemy player
            
jumperBehaviour :: Enemy -> Player -> Float -> Enemy
jumperBehaviour enemy@(Enemy{..}) player@(Player{..}) time = enemy {enemyLocation = enemyLocation + (dx, (-dy)), enemyDirection = newDirection, enemyTimer = enemyTimer'}
        where
            enemyTimer'    = if (enemyTimer < 0 ) then 4 else enemyTimer - time
            enemySpeed     = (enemyTimer * 200) * time            
            newDirection   = if (enemyTimer' > 0) then enemyDirection else directionToPlayer enemy player
            (dx, dy)       = mulSV enemySpeed $ unitVectorAtAngle (newDirection)
            

directionToPlayer :: Enemy -> Player -> Float
directionToPlayer enemy@(Enemy{..}) player@(Player{..}) = (if snd vectorToPlayer < 0 then id else negate) (angleVV vectorToPlayer (1, 0))
        where
            vectorToPlayer = (playerLocation - enemyLocation)
            
radToDeg x = x * 180 / pi
degToRad x = x * pi / 180
            
