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
-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) = world {player = updatedPlayer, particles = newParticles, rndGen = newStd, bullets = newBullets, enemies = newEnemies, enemyTimer = newEnemyTimer}
    where
        updatedPlayer                       = updatePlayer time world        
        (thrustParticles, r1)               = createThrustParticles world
        newParticles                        = thrustParticles ++ updateParticles time particles
        newBullets                          = shootBullet time shootAction player bullets
        (enemies1, newEnemyTimer, newStd)   = spawnRandomEnemy time world r1
        newEnemies                          = updateEnemies time player enemies1
      
--------------Player stuff -----------------------------------      
updatePlayer :: Float -> World -> Player
updatePlayer time world@(World{..}) = 
    do
    let p1 = rotatePlayer player rotateAction
    let p2 = movePlayer p1 movementAction
    let p3 = wrapPlayer p2 (800, 640)
    p3 {playerLocation = playerLocation p3 + (mulSV time $ playerSpeed p3)}

        
rotatePlayer :: Player -> RotateAction -> Player
rotatePlayer player NoRotation  = player
rotatePlayer player RotateLeft  = player {direction = direction player + rotationSpeed}
rotatePlayer player RotateRight = player {direction = direction player - rotationSpeed}

movePlayer :: Player -> MovementAction -> Player
movePlayer player@(Player {playerSpeed, direction}) NoMovement = player {playerSpeed = playerSpeed * (0.96, 0.96)}
movePlayer player@(Player {playerSpeed, direction}) Thrust =
    do 
        let sp1 = playerSpeed * (0.97, 0.97) + rotateV (pi * direction / 180)(playerAcceleration, 0)        
        let maxSpeedReached = magV sp1 >= maxSpeed
        let newSpeed = if maxSpeedReached then setMagnitudeVS sp1 maxSpeed else sp1
        player {playerSpeed = newSpeed}
    


wrapPlayer :: Player -> (Int, Int) -> Player
wrapPlayer player@(Player {playerLocation}) (w, h) = player { playerLocation = (wrap (-1000) 1000 (fst playerLocation), wrap (-1000) 1000 (snd playerLocation))}
    where wrap low high x | x < low  = low
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
        | enemyTimer' > 0  = (enemies,            enemyTimer',  stdGen)
        | otherwise        = (enemy:enemies, 2 + timeDiff, r2)
    where
        enemyTimer'     = enemyTimer - ellapsed        
        (timeDiff, r1)  = randomR ((-0.75), 0.75) rndGen
        (enemy,    r2) = spawnEnemyAtRandomLocation r1
        
updateEnemies :: Float -> Player -> [Enemy] -> [Enemy]
updateEnemies time player xs = map (\x -> (updateEnemy x) x player time) xs