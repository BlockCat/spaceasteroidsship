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
import Multiplier
import Location

rotationSpeed = 6

maxSpeed = 1000
playerAcceleration = 15
bulletVelocity = 1000
spawnDistance = 500
hitBox = 15

-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) | fst $ isHit hitBox player enemies = (emptyWorld world) {particles = particles ++ fst (explosion (playerLocation player) rndGen)}
                                    | otherwise            = world {
                                                                    player          = updatedPlayer1, 
                                                                    particles       = newParticles, 
                                                                    rndGen          = newStd, 
                                                                    bullets         = newBullets, 
                                                                    multipliers     = newMultipliers3, 
                                                                    enemies         = newEnemies,
                                                                    enemySpawnTimer = newEnemyTimer, 
                                                                    multiplierTimer = newMultiTimer, 
                                                                    playerScore     = newScore,
                                                                    scoreMultiplier = newMultiplier
                                                                    }
    where
        updatedPlayer                        = updatePlayer time world        
        (thrustParticles, r1)                = createThrustParticles world
        newParticles                         = thrustParticles ++ updateParticles time particles
        (updatedPlayer1, newBullets)         = shootBullet time shootAction updatedPlayer (filterOutOfRange bullets)
        (newMultipliers1, newMultiTimer, r2) = spawnRandomMultiplier time world r1
        (enemies1, newEnemyTimer, r3)        = spawnRandomEnemy time world enemies r2 simpleBehaviour
        (enemies2, _            , newStd)    = if playerScore > 100 then spawnRandomEnemy time world enemies1 r3 jumperBehaviour else (enemies1, newEnemyTimer, r3)
        enemies3                             = checkEnemies bullets enemies2
        (multiplierAdd, newMultipliers2)     = hitMultiplier player newMultipliers1
        newMultipliers3                      = foldr (\b acc -> snd $ isHit 15 b acc) newMultipliers2 bullets
        newMultiplier                        = scoreMultiplier + multiplierAdd
        newScore                             = playerScore + scoreMultiplier * (length enemies2 - length enemies3) 
        newEnemies                           = updateEnemies time player enemies3
        
--------------Player stuff -----------------------------------      
updatePlayer :: Float -> World -> Player
updatePlayer time World{..} = p {playerLocation = location p + mulSV time (playerSpeed p), shootTimer = shootTimer', canShoot = canShoot'}
    where
        p           = (wrapPlayer . movePlayer movementAction . rotatePlayer rotateAction) player
        shootTimer' = if shootTimer p - time > 0 && (not.canShoot) p then shootTimer p - time else 0
        canShoot'   = shootTimer' <= 0
                    
                         

      
        
rotatePlayer :: RotateAction -> Player-> Player
rotatePlayer RotateLeft  player = player {direction = direction player + rotationSpeed}
rotatePlayer RotateRight player = player {direction = direction player - rotationSpeed}
rotatePlayer _           player = player

movePlayer :: MovementAction -> Player -> Player
movePlayer NoMovement player@(Player {playerSpeed, direction})  = player {playerSpeed = mulSV 0.96 playerSpeed}
movePlayer _          player@(Player {playerSpeed, direction})  = player {playerSpeed = newSpeed}
    where 
        sp1      = mulSV 0.97 playerSpeed + rotateV (pi * direction / 180)(playerAcceleration, 0)        
        newSpeed | magV sp1 >= maxSpeed = (mulSV maxSpeed . normalizeV) sp1
                 | otherwise            = sp1

wrapPlayer :: Player -> Player
wrapPlayer Player {..} = Player{playerLocation = (wrap (-1000) 1000 x, wrap (-1000) 1000 y), ..}
    where (x, y)          = playerLocation    
          wrap low high x | x < low  = low
                          | x > high = high
                          | otherwise = x      

shootBullet :: Float -> ShootAction -> Player -> [Bullet] -> (Player, [Bullet])
shootBullet time DontShoot p               xs = (p, map (moveBullet time) xs)
shootBullet time Shoot     p@(Player {..}) xs | canShoot = (p {canShoot = False, shootTimer = 0.34},(bulletPos 21 (-11)) :(bulletPos (-21) (-11)) : (map (moveBullet time) xs))
                                              | otherwise  = (p, map (moveBullet time) xs)
    where 
        (x, y)        = playerLocation        
        bulletPos i j = Bullet (x+k, y+l) (rotateV (direction*pi / 180) (bulletVelocity, 0)) direction
              where (k, l) = rotateV (degToRad (90+direction)) (i, j)


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

isHit :: (Location a, Location b) => Float -> a -> [b] -> (Bool, [b])
isHit hitRadius p1 xs | hasCollided = (True, newList)
                      | otherwise   = (False, xs)
    where
        list        = map (\x -> (hitCheck hitRadius p1 x, x)) xs
        hasCollided = or $ map hit list
        hit (x, _)  = x
        newList     = (map snd . filter (not.hit)) list      
                         
hitCheck :: (Location a, Location b) => Float -> a -> b -> Bool
hitCheck hitRadius p1 p2 = (magV (location p1 - location p2)) < hitRadius

hitMultiplier :: (Location a, Location b) => a -> [b] -> (Int, [b])
hitMultiplier p1 xs | collided  = (1, newList)
                    | otherwise = (0, newList)
    where
    (collided, newList) = isHit 30 p1 xs
--------------Enemy  start----------------------------------
spawnRandomEnemy :: Float -> World -> [Enemy] -> StdGen -> (Enemy -> Player -> Float -> Enemy) -> ([Enemy], Float, StdGen)
spawnRandomEnemy ellapsed world@(World {..}) xs stdGen behaviour
        | enemyTimer' > 0  = (xs ,  enemyTimer', stdGen)
        | otherwise        = (enemies', 2 + timeDiff, r2)
    where
        enemyTimer'     = enemySpawnTimer - ellapsed
        (loc     , r1)  = randomLocation rndGen
        (timeDiff, r2)  = randomR (-0.75, 0.75) r1
        enemy           = Enemy loc 0 (0, 0) 4 behaviour
        enemies'        | shouldSpawn enemy player = enemy:xs
                        | otherwise                = xs        -- Verplaatsen naar random, omdat die alleen random plek genereerd. Verder zo doen dat als niet shouldspawn, dan nieuwe enemy genereren.
                        
updateEnemies :: Float -> Player -> [Enemy] -> [Enemy]
updateEnemies time player = moveEnemies . updatedEnemies
    where
        updatedEnemies = map (\enemy@Enemy{..} -> updateEnemy enemy player time) 

checkEnemies :: [Bullet] -> [Enemy] -> [Enemy]
checkEnemies bullets = filter (\x -> (not (fst (isHit 20 x bullets)))) 

shouldSpawn :: Enemy -> Player -> Bool
shouldSpawn Enemy{..} Player{..} = magV (enemyLocation - playerLocation) > spawnDistance   


spawnRandomMultiplier :: Float -> World -> StdGen -> ([Multiplier], Float, StdGen)
spawnRandomMultiplier ellapsed world@(World {..}) stdGen
        | multiplierTimer' > 0  = (multipliers ,  multiplierTimer', stdGen)
        | otherwise             = (multipliers', 2 + timeDiff, r2)
    where
        multiplierTimer' = multiplierTimer - ellapsed        
        (timeDiff, r1)   = randomR (-0.75, 0.75) rndGen
        (multiplier, r2) = spawnMultiplierAtRandomLocation r1
        multipliers'     = multiplier:multipliers 
        
