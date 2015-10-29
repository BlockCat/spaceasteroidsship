{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}

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
                                    | otherwise            = world {player = updatedPlayer, particles = newParticles, rndGen = newStd, bullets = newBullets, enemies = newEnemies, enemySpawnTimer = newEnemyTimer, playerScore = newScore}
    where
        updatedPlayer                       = updatePlayer time world        
        (thrustParticles, r1)               = createThrustParticles world
        newParticles                        = thrustParticles ++ updateParticles time particles
        newBullets                          = shootBullet time shootAction player (filterOutOfRange bullets)
        (enemies1, newEnemyTimer, newStd)   = spawnRandomEnemy time world r1
        enemies2                            = checkEnemies bullets enemies1
        newScore                            = playerScore + 10 * ((length enemies1) - (length enemies2)) 
        newEnemies                          = updateEnemies time player enemies2
        
--------------Player stuff -----------------------------------      
updatePlayer :: Float -> World -> Player
updatePlayer time World{..} = p {playerLocation = playerLocation p + mulSV time (playerSpeed p)}
    where
        p  = (wrapPlayer . movePlayer movementAction . rotatePlayer rotateAction) player      
                         
isHit :: Player -> [Enemy] -> Bool
isHit player = any (hitCheck player) 
                         
hitCheck :: Player -> Enemy -> Bool
hitCheck Player{..} Enemy{..} = distance < hitBox   
    where 
        distance = magV (enemyLocation - playerLocation)                          

        
rotatePlayer :: RotateAction -> Player-> Player
rotatePlayer RotateLeft  player = player {direction = direction player + rotationSpeed}
rotatePlayer RotateRight player = player {direction = direction player - rotationSpeed}
rotatePlayer _           player = player

movePlayer :: MovementAction -> Player -> Player
movePlayer NoMovement player@(Player {playerSpeed, direction})  = player {playerSpeed = mulSV 0.96 playerSpeed}
movePlayer _          player@(Player {playerSpeed, direction})  = player {playerSpeed = newSpeed}
    where 
        sp1      = mulSV 0.97 playerSpeed + rotateV (pi * direction / 180)(playerAcceleration, 0)        
        newSpeed | magV sp1 >= maxSpeed = setMagnitudeVS maxSpeed sp1
                 | otherwise            = sp1

wrapPlayer :: Player -> Player
wrapPlayer Player {..} = Player{playerLocation = (wrap (-1000) 1000 x, wrap (-1000) 1000 y), ..}
    where (x, y)          = playerLocation    
          wrap low high x | x < low  = low
                          | x > high = high
                          | otherwise = x      

shootBullet :: Float -> ShootAction -> Player -> [Bullet] -> [Bullet]
shootBullet time DontShoot _ = map (moveBullet time) 
shootBullet time _ (Player {..}) = (b :) . map (moveBullet time) 
    where b = Bullet playerLocation (rotateV (direction*pi / 180) (bulletVelocity, 0)) direction

createThrustParticles :: World -> ([Particle], StdGen)
createThrustParticles (World{player, movementAction, rndGen}) | movementAction == Thrust = randParticles
                                                              | otherwise                = ([], rndGen)
    where 
        speedVar      = 1.0
        degreeVar     = 45
        lifeTimeVar   = 0.1
        lifeTime      = 0.115
        speed         = 0.3
        pict          = (color yellow . circleSolid) 2        
        particle      = Particle (playerLocation player) (playerSpeed player * (negate speed, negate speed)) lifeTime pict
        randParticles = generateRandom rndGen (randomizedParticle speedVar degreeVar lifeTimeVar particle) 30
    
--------------Player end -----------------------------------   

setMagnitudeVS :: Float -> Vector -> Vector
setMagnitudeVS mag = mulSV mag . normalizeV

--------------Enemy  start----------------------------------
spawnRandomEnemy :: Float -> World -> StdGen -> ([Enemy], Float, StdGen)
spawnRandomEnemy ellapsed world@(World {..}) stdGen
        | enemyTimer' > 0  = (enemies ,  enemyTimer', stdGen)
        | otherwise        = (enemies', 2 + timeDiff, r2)
    where
        enemyTimer'     = enemySpawnTimer - ellapsed        
        (timeDiff, r1)  = randomR (-0.75, 0.75) rndGen
        (enemy,    r2)  = spawnEnemyAtRandomLocation r1
        enemies'        | shouldSpawn enemy player = enemy:enemies 
                        | otherwise                = enemies        -- Verplaatsen naar random, omdat die alleen random plek genereerd. Verder zo doen dat als niet shouldspawn, dan nieuwe enemy genereren.
        
updateEnemies :: Float -> Player -> [Enemy] -> [Enemy]
updateEnemies time player = moveEnemies . updatedEnemies
    where
        updatedEnemies = map (\enemy@Enemy{..} -> updateEnemy enemy player time) 

checkEnemies :: [Bullet] -> [Enemy] -> [Enemy]
checkEnemies bullets enemies = filter (not.hitBullet bullets) enemies

hitBullet :: [Bullet] -> Enemy -> Bool
hitBullet bullets Enemy{..} = or $ map (\x -> hitBox > distance x) bullets
    where
        hitBox   = 20
        distance Bullet{..} = magV (bulletLocation - enemyLocation)     

shouldSpawn :: Enemy -> Player -> Bool
shouldSpawn Enemy{..} Player{..} = magV (enemyLocation - playerLocation) > spawnDistance   
