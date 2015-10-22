{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards       #-}
{-# LANGUAGE ParallelListComp                                                #-}

module Controller.Time (
    timeHandler
) where

import Control.Arrow ((>>>))
import Data.Fixed (mod')

import Data.List

import Graphics.Gloss
--import Graphics.Gloss.Geometry

import System.Random

import Model


rotationSpeed = 6
maxSpeed = 19
-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {player, rotateAction, movementAction, bullets, shootAction, ..}) = do
        let p1 = rotatePlayer player rotateAction
        let p2 = movePlayer p1 movementAction
        let p3 = wrapPlayer p2 (800, 640)
        let newPlayer  = updatePlayer p3      
        let newBullets = updateBullets shootAction player bullets
        world {player = newPlayer, bullets = newBullets}

        
rotatePlayer :: Player -> RotateAction -> Player
rotatePlayer player NoRotation  = player
rotatePlayer player RotateLeft  = player {direction = (direction player) + rotationSpeed}
rotatePlayer player RotateRight = player {direction = (direction player) - rotationSpeed}

movePlayer :: Player -> MovementAction -> Player
movePlayer player@(Player {dx, dy, direction}) NoMovement = player {dx = dx *0.95, dy = dy *0.95}
movePlayer player@(Player {dx, dy, direction}) Thrust =
        do 
        let dx1 = dx * 0.97 + cos (direction * pi / 180) * 0.6
        let dy1 = dy * 0.97 + sin (direction * pi / 180) * 0.6
        let maxSpeedReached = (totalSpeedSquared dx1 dy1) >= (maxSpeed * maxSpeed)
        let dx2 = if maxSpeedReached then dx else dx1
        let dy2 = if maxSpeedReached then dy else dy1        
        player {dx = dx2, dy = dy2}

    where
    --clipSpeed df a      = (df/ totalSpeedSquared df a) * maxSpeed
    clipSpeed df a      = df --(df/ maxSpeed) -- * maxSpeed
    totalSpeedSquared dx' dy' = (dx' * dx') + (dy' * dy')

wrapPlayer :: Player -> (Int, Int) -> Player
wrapPlayer player@(Player {x, y}) (w, h) = player { x = wrap (-450) 450 x, y = wrap (-320) 320 y}
    where wrap low high x | x < low  = high
                          | x > high = low
                          | otherwise = x
    
updatePlayer :: Player -> Player
updatePlayer player@(Player {x, y, dx, dy}) = player {x = x+dx, y=y+dy}

updateBullets :: ShootAction -> Player -> [Bullet] -> [Bullet]
updateBullets Shoot player@(Player {x, y, dx, dy}) bs = let b = Bullet x y dx dy in b:bs
updateBullets _ _ bs = bs