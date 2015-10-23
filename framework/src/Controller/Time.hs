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

import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle

import System.Random

import Model


rotationSpeed = 6
maxSpeed      = 19
bulletSpeedConstant = 5
-- | Time handling

timeHandler :: Float -> World -> World
timeHandler time world@(World {..}) = do 
        let updatedPlayer = updatePlayer world
        let newBullets = updateBullets shootAction player $ map moveBullet bullets
        world {player = updatedPlayer, bullets = newBullets}
        
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
wrapPlayer player@(Player {playerLocation}) (w, h) = player { playerLocation = (wrap (-450) 450 (fst playerLocation), wrap (-320) 320 (snd playerLocation))}
    where wrap low high x | x < low  = high
                          | x > high = low
                          | otherwise = x

updateBullets :: ShootAction -> Player -> [Bullet] -> [Bullet]
updateBullets Shoot (Player {..}) bs = let b = Bullet playerLocation playerSpeed direction in b:bs
updateBullets _ _ bs = bs

moveBullet :: Bullet -> Bullet
moveBullet Bullet{..} = Bullet{bulletLocation = location, ..}
    where location = (x + speedX + bulletSpeedConstant * (cos dir), y + speedY + bulletSpeedConstant * (sin dir))
          (x,y)            = bulletLocation
          (speedX, speedY) = bulletSpeed
          dir              = degToRad bulletDir