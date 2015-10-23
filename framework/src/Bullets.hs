{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Bullets where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point 
import Graphics.Gloss

    
data Bullet = Bullet {
        bulletX         :: Float,
        bulletY         :: Float,
        bulletDir       :: Float
    }
                
drawBullets :: [Bullet] -> Picture
drawBullets xs = Pictures $ map drawShoot xs 

drawShoot :: Bullet -> Picture
drawShoot Bullet{..} =  pictures [drawBullet 24 13, drawBullet (-24) 13]
    where drawBullet x y = (color yellow . translate (bulletX) (bulletY) . rotate (90-bulletDir :: Float) . translate x y . circleSolid) 2