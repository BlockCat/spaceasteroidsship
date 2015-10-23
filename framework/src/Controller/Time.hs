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
--import Graphics.Gloss.Geometry

import System.Random

import Model
import RandomUtils
import Particles
import Bullets

rotationSpeed = 6
maxSpeed = 10
-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) = world {player = updatedPlayer, particles = newParticles, rndGen = r1, bullets = newBullets}
    where
        updatedPlayer         = updatePlayer world        
        (thrustParticles, r1) = createThrustParticles world
        newParticles          = thrustParticles ++ updateParticles time particles
        newBullets            = updateBullets shootAction player bullets
        
      
--------------Player stuff -----------------------------------      
updatePlayer :: World -> Player
updatePlayer world@(World{..}) = 
    do
    let p1 = rotatePlayer player rotateAction
    let p2 = movePlayer p1 movementAction
    let p3 = wrapPlayer p2 (800, 640)
    p3 {playerLocation = playerLocation p3 + playerSpeed p3}

        
rotatePlayer :: Player -> RotateAction -> Player
rotatePlayer player NoRotation  = player
rotatePlayer player RotateLeft  = player {direction = (direction player) + rotationSpeed}
rotatePlayer player RotateRight = player {direction = (direction player) - rotationSpeed}

movePlayer :: Player -> MovementAction -> Player
movePlayer player@(Player {playerSpeed, direction}) NoMovement = player {playerSpeed = playerSpeed * (0.96, 0.96)}
movePlayer player@(Player {playerSpeed, direction}) Thrust =
    do 
        let sp1 = playerSpeed * (0.97, 0.97) + rotateV (direction * pi / 180) (0.6, 0)        
        let maxSpeedReached = magV sp1 >= maxSpeed
        let newSpeed = if (maxSpeedReached) then rotateV (direction * pi / 180) (maxSpeed, 0) else sp1
        player {playerSpeed = newSpeed}
    

wrapPlayer :: Player -> (Int, Int) -> Player
wrapPlayer player@(Player {playerLocation}) (w, h) = player { playerLocation = (wrap (-1000) 1000 (fst playerLocation), wrap (-1000) 1000 (snd playerLocation))}
    where wrap low high x | x < low  = low
                          | x > high = high
                          | otherwise = x


createThrustParticles :: World -> ([Particle], StdGen)
createThrustParticles (World{player, movementAction, rndGen}) = if movementAction == Thrust then randParticles else ([], rndGen)
    where 
        speedVar    = 1.0
        degreeVar   = 180
        lifeTimeVar = 0.1
        lifeTime    = 0.015
        speed       = 0.3
        pict = color yellow $ circleSolid 2
        
        particle = createParticle (playerLocation player) ((playerSpeed player) * (negate speed, negate speed)) lifeTime $ pict
        randParticles = generateRandom rndGen (randomizedParticle speedVar degreeVar lifeTimeVar particle) 30
    
--------------Player end -----------------------------------    

--Is dit niet eerder een shootBullet functie?
updateBullets :: ShootAction -> Player -> [Bullet] -> [Bullet]
updateBullets Shoot (Player {..}) bs = let b = Bullet x y direction in b:bs
    where (x,y)    = playerLocation
updateBullets _ _ bs = bs
