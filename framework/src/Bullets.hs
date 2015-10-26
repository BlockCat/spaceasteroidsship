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
drawBullets xs = Pictures $ map drawShoot xs 

drawShoot :: Bullet -> Picture
drawShoot Bullet{..} =  pictures [drawBullet 24 13, drawBullet (-24) 13]
    where drawBullet i j = (color yellow . translate x y . rotate (90-bulletDir :: Float) . translate i j . circleSolid) 2
          (x, y)         = bulletLocation

-- The shooting and movement of bullets
bulletSpeedConstant = 5

moveBullet :: Float -> Bullet -> Bullet
moveBullet time Bullet{..} = Bullet{bulletLocation = location, ..}
    where location = bulletLocation + (mulSV time bulletSpeed)