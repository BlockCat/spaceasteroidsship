{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Stars where

import System.Random
import Control.Monad        (replicateM)
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point

-- | Game state

data Star = Star {
        -- Event queue
        location        :: Point,
        depth           :: Float
    }
 
createStar :: StdGen -> IO Star
createStar rndGen = 
    do
        x <- getStdRandom $ randomR (-400, 400)
        y <- getStdRandom $ randomR (-400, 400)
        z <- getStdRandom $ randomR (5, 16)
        return $ Star (x, y) z
            
generateStarField :: StdGen -> Int -> IO [Star]
generateStarField rndGen n = replicateM n (createStar rndGen)
        