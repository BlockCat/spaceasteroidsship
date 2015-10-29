{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Bullets where

import System.Random
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
    
data Bullet = Bullet {
        bulletLocation  :: Point,
        bulletSpeed     :: Vector,
        bulletDir       :: Float
    }

-- The drawing of the bullets    
drawBullets :: [Bullet] -> Picture
drawBullets = Pictures . map drawShoot

drawShoot :: Bullet -> Picture
drawShoot Bullet{..} =  (color yellow . translate x y . rotate (90-bulletDir :: Float) . circleSolid) 2
    where (x, y) = bulletLocation

-- The shooting and movement of bullets
bulletSpeedConstant = 5

moveBullet :: Float -> Bullet -> Bullet
moveBullet time Bullet{..} = Bullet{bulletLocation = location, ..}
    where location = bulletLocation + mulSV time bulletSpeed

bulletOutOfWindow :: Bullet -> Bool
bulletOutOfWindow Bullet{..} | x > 1500 || x < (-1500) || y < (-1500) || y > 1500 = True
                             | otherwise                                          = False
    where (x, y) = bulletLocation
    
filterOutOfRange :: [Bullet] -> [Bullet]
filterOutOfRange = filter (not . bulletOutOfWindow)
