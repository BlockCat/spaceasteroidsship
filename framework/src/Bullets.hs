{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Bullets where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Geometry.Angle
    
data Bullet = Bullet {
        bulletLocation  :: Point,
        bulletSpeed     :: Vector,
        bulletDir       :: Float
    }

-- The drawing of the bullets    
drawBullets :: [Bullet] -> Picture
drawBullets xs = Pictures $ map drawShoot xs 

drawShoot :: Bullet -> Picture
drawShoot Bullet{..} =  pictures [drawBullet 24 13, drawBullet (-24) 13]
    where drawBullet i j = (color yellow . translate x y . rotate (90-bulletDir :: Float) . translate i j . circleSolid) 2
          (x, y)         = bulletLocation

-- The shooting and movement of bullets
bulletSpeedConstant = 5

moveBullet :: Bullet -> Bullet
moveBullet Bullet{..} = Bullet{bulletLocation = location, ..}
    where location = (x + speedX + bulletSpeedConstant * cos dir, y + speedY + bulletSpeedConstant * sin dir)
          (x,y)            = bulletLocation
          (speedX, speedY) = bulletSpeed
          dir              = degToRad bulletDir

bulletOutOfWindow :: Bullet -> Bool
bulletOutOfWindow Bullet{..} | (x > 1500 || x < (-1500) || y < (-1500) || y > 1500) = True
                             | otherwise                                            = False
    where (x, y) = bulletLocation
    
filterOutOfRange :: [Bullet] -> [Bullet]
filterOutOfRange bs = filter (not . bulletOutOfWindow) bs