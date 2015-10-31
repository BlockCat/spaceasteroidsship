{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Multiplier where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Player

data Multiplier = Multiplier {
        multiplierLocation  :: Point
    }

drawMultipliers :: [Multiplier] -> Picture
drawMultipliers xs = Pictures $ map drawMultiplier xs

drawMultiplier :: Multiplier -> Picture
drawMultiplier Multiplier{..} = translate x y (color green triangle)
    where 
        triangle = polygon [(-10, -5), (10, -5), (0, 10)]
        (x, y)   = multiplierLocation              
              
spawnMultiplierAtRandomLocation :: StdGen -> (Multiplier, StdGen)
spawnMultiplierAtRandomLocation stdGen = (Multiplier (x', y'), r2)
    where
        (x', r1) = randomR (-1000, 1000) stdGen 
        (y', r2) = randomR (-1000, 1000) r1
