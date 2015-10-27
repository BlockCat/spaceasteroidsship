{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))
import Data.Fixed (mod')
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import Data.List
import Control.Monad

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import System.Random

import Model
import Player
import RandomUtils
import Particles
import Bullets
import Enemies

rotationSpeed = 6

maxSpeed = 1000
playerAcceleration = 15
bulletVelocity = 1000
spawnDistance = 500

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) | isHit player enemies    = (emptyWorld world) {particles = particles ++ fst (explosion (playerLocation player) rndGen)}--error "getBetter"
                                    | otherwise               = world {player = updatedPlayer, particles = newParticles, rndGen = newStd, bullets = newBullets, enemies = newEnemies, enemyTimer = newEnemyTimer}
    where
        updatedPlayer                       = updatePlayer time world        
        (thrustParticles, r1)               = createThrustParticles world
        newParticles                        = thrustParticles ++ updateParticles time particles
        newBullets                          = shootBullet time shootAction player bullets
        (enemies1, newEnemyTimer, newStd)   = spawnRandomEnemy time world r1
        newEnemies                          = updateEnemies time player enemies1        
        
--------------Player stuff -----------------------------------      
updatePlayer :: Float -> World -> Player
updatePlayer time World{..} = p3 {playerLocation = playerLocation p3 + (mulSV time $ playerSpeed p3)}
    where
        p1 = rotatePlayer player rotateAction
        p2 = movePlayer p1 movementAction
        p3 = wrapPlayer p2 (800, 640)        
                         
isHit :: Player -> [Enemy] -> Bool
isHit player enemies = or $ map (hitCheck player) enemies
                         
hitCheck :: Player -> Enemy -> Bool
hitCheck Player{..} Enemy{..} | distance < 15 = True
                              | otherwise     = False
    where 
        distance = magV (enemyLocation - playerLocation)

        
rotatePlayer :: Player -> RotateAction -> Player
rotatePlayer player NoRotation  = player
rotatePlayer player RotateLeft  = player {direction = direction player + rotationSpeed}
rotatePlayer player RotateRight = player {direction = direction player - rotationSpeed}

movePlayer :: Player -> MovementAction -> Player
movePlayer player@(Player {playerSpeed, direction}) NoMovement = player {playerSpeed = playerSpeed * (0.96, 0.96)}
movePlayer player@(Player {playerSpeed, direction}) Thrust = player {playerSpeed = newSpeed}
    where 
        sp1 = playerSpeed * (0.97, 0.97) + rotateV (pi * direction / 180)(playerAcceleration, 0)        
        maxSpeedReached = magV sp1 >= maxSpeed
        newSpeed = if maxSpeedReached then setMagnitudeVS sp1 maxSpeed else sp1
   


wrapPlayer :: Player -> (Int, Int) -> Player
wrapPlayer player@(Player {playerLocation}) (w, h) = player { playerLocation = (wrap (-1000) 1000 x, wrap (-1000) 1000 y)}
    where (x, y)          = playerLocation    
          wrap low high x | x < low  = low
                          | x > high = high
                          | otherwise = x      

shootBullet :: Float -> ShootAction -> Player -> [Bullet] -> [Bullet]
shootBullet time Shoot (Player {..}) bs = b: map (moveBullet time) bs
    where b = Bullet playerLocation (rotateV (direction*pi / 180) (bulletVelocity, 0)) direction
shootBullet time _ _ bs = map (moveBullet time) bs


createThrustParticles :: World -> ([Particle], StdGen)
createThrustParticles (World{player, movementAction, rndGen}) = if movementAction == Thrust then randParticles else ([], rndGen)
    where 
        speedVar    = 1.0
        degreeVar   = 45
        lifeTimeVar = 0.1
        lifeTime    = 0.115
        speed       = 0.3
        pict = color yellow $ circleSolid 2
        
        particle = createParticle (playerLocation player) (playerSpeed player * (negate speed, negate speed)) lifeTime pict
        randParticles = generateRandom rndGen (randomizedParticle speedVar degreeVar lifeTimeVar particle) 30
    
--------------Player end -----------------------------------   

setMagnitudeVS :: Vector -> Float -> Vector
setMagnitudeVS vector mag = mulSV mag $ normalizeV vector
--------------Enemy  start----------------------------------
spawnRandomEnemy :: Float -> World -> StdGen -> ([Enemy], Float, StdGen)
spawnRandomEnemy ellapsed world@(World {..}) stdGen
        | enemyTimer' > 0  = (enemies ,  enemyTimer', stdGen)
        | otherwise        = (enemies', 2 + timeDiff, r2)
    where
        enemyTimer'     = enemyTimer - ellapsed        
        (timeDiff, r1)  = randomR ((-0.75), 0.75) rndGen
        (enemy,    r2)  = spawnEnemyAtRandomLocation r1
        enemies'        = if shouldSpawn enemy player then enemy:enemies else enemies
        
updateEnemies :: Float -> Player -> [Enemy] -> [Enemy]
updateEnemies time player xs = map (\x -> (updateEnemy x) x player time) xs

shouldSpawn :: Enemy -> Player -> Bool
shouldSpawn Enemy{..} Player{..} | magV (enemyLocation - playerLocation) > spawnDistance = True
                                 | otherwise                                 = False                  

{-spawnEnemies :: Player -> Enemy
spawnEnemies Player{..} = Enemy (x-50, y+50) (0,0) 0 
    where (x, y) = playerLocation-}

