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
hitBox = 15

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) | isHit player enemies = (emptyWorld world) {particles = particles ++ (fst $ explosion (playerLocation player) rndGen)}
                                    | otherwise            = world {player = updatedPlayer, particles = newParticles, rndGen = newStd, bullets = newBullets, enemies = newEnemies, enemySpawnTimer = newEnemyTimer}
    where
        updatedPlayer                       = updatePlayer time world        
        (thrustParticles, r1)               = createThrustParticles world
        newParticles                        = thrustParticles ++ updateParticles time particles
        newBullets                          = shootBullet time shootAction player (filterOutOfRange bullets)
        (enemies1, newEnemyTimer, newStd)   = spawnRandomEnemy time world r1
        newEnemies                          = updateEnemies time player enemies1        
        
--------------Player stuff -----------------------------------      
updatePlayer :: Float -> World -> Player
updatePlayer time World{..} = p {playerLocation = playerLocation p + (mulSV time $ playerSpeed p)}
    where
        p  = (wrapPlayer . movePlayer movementAction . rotatePlayer rotateAction) player      
                         
isHit :: Player -> [Enemy] -> Bool
isHit player enemies = or $ map (hitCheck player) enemies
                         
hitCheck :: Player -> Enemy -> Bool
hitCheck Player{..} Enemy{..} | distance < hitBox = True
                              | otherwise         = False
    where 
        distance = magV (enemyLocation - playerLocation)

        
rotatePlayer :: RotateAction -> Player-> Player
rotatePlayer NoRotation  player  = player
rotatePlayer RotateLeft  player = player {direction = direction player + rotationSpeed}
rotatePlayer RotateRight player = player {direction = direction player - rotationSpeed}

movePlayer :: MovementAction -> Player -> Player
movePlayer NoMovement player@(Player {playerSpeed, direction})  = player {playerSpeed = mulSV 0.96 playerSpeed}
movePlayer Thrust     player@(Player {playerSpeed, direction})  = player {playerSpeed = newSpeed}
    where 
        sp1      = mulSV 0.97 playerSpeed + rotateV (pi * direction / 180)(playerAcceleration, 0)        
        newSpeed | magV sp1 >= maxSpeed = setMagnitudeVS sp1 maxSpeed 
                 | otherwise            = sp1

wrapPlayer :: Player -> Player
wrapPlayer player@(Player {playerLocation}) = player { playerLocation = (wrap (-1000) 1000 x, wrap (-1000) 1000 y)}
    where (x, y)          = playerLocation    
          wrap low high x | x < low  = low
                          | x > high = high
                          | otherwise = x      

shootBullet :: Float -> ShootAction -> Player -> [Bullet] -> [Bullet]
shootBullet time Shoot (Player {..}) bs = b: map (moveBullet time) bs
    where b = Bullet playerLocation (rotateV (direction*pi / 180) (bulletVelocity, 0)) direction
shootBullet time _ _ bs = map (moveBullet time) bs


createThrustParticles :: World -> ([Particle], StdGen)
createThrustParticles (World{player, movementAction, rndGen}) | movementAction == Thrust = randParticles
                                                              | otherwise                = ([], rndGen)
    where 
        speedVar    = 1.0
        degreeVar   = 45
        lifeTimeVar = 0.1
        lifeTime    = 0.115
        speed       = 0.3
        pict        = color yellow $ circleSolid 2
        
        particle      = Particle (playerLocation player) (playerSpeed player * (negate speed, negate speed)) lifeTime pict
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
        enemyTimer'     = enemySpawnTimer - ellapsed        
        (timeDiff, r1)  = randomR ((-0.75), 0.75) rndGen
        (enemy,    r2)  = spawnEnemyAtRandomLocation r1
        enemies'        = if shouldSpawn enemy player then enemy:enemies else enemies
        
updateEnemies :: Float -> Player -> [Enemy] -> [Enemy]
updateEnemies time player xs = map (\x -> (updateEnemy x) x player time) xs

shouldSpawn :: Enemy -> Player -> Bool
shouldSpawn Enemy{..} Player{..} | magV (enemyLocation - playerLocation) > spawnDistance = True
                                 | otherwise                                             = False                  
