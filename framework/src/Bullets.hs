{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Bullets where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point 
import Graphics.Gloss

    
data Bullet = Bullet {
        bulletLocation  :: Point,
        bulletSpeed     :: Vector,
        bulletDir       :: Float
    }
                
drawBullets :: [Bullet] -> Picture
drawBullets xs = Pictures $ map drawShoot xs 

drawShoot :: Bullet -> Picture
drawShoot Bullet{..} =  pictures [drawBullet 24 13, drawBullet (-24) 13]
    where drawBullet i j = (color yellow . translate x y . rotate (90-bulletDir :: Float) . translate i j . circleSolid) 2
          (x, y)         = bulletLocation