{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Stars where

import System.Random
import Control.Monad        (replicateM)
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point

import RandomUtils

-- | Game state

data Star = Star {
        -- Event queue
        location        :: Point,
        depth           :: Float
    }
 
createStar :: StdGen -> (Star, StdGen)
createStar rndGen = (Star (x, y) z, r3)
    where
        (x, r1) = randomR (-400, 400) rndGen
        (y, r2) = randomR (-400, 400) r1
        (z, r3) = randomR (5, 16) r2
        
            
generateStarField :: StdGen -> Int -> ([Star], StdGen)
generateStarField rndGen n = generateRandom rndGen createStar n