{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Multiplier where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Player
import Location

data Multiplier = Multiplier {
        multiplierLocation  :: Point
    }
instance Location Multiplier where {
    location = multiplierLocation
    }
drawMultipliers :: Picture -> [Multiplier] -> Picture
drawMultipliers pict xs = Pictures $ map (drawMultiplier pict) xs

drawMultiplier :: Picture -> Multiplier -> Picture
drawMultiplier picture Multiplier{..} = translate x y picture
    where         
        (x, y)   = multiplierLocation              
              
spawnMultiplierAtRandomLocation :: StdGen -> (Multiplier, StdGen)
spawnMultiplierAtRandomLocation stdGen = (Multiplier (x', y'), r2)
    where
        (x', r1) = randomR (-1000, 1000) stdGen 
        (y', r2) = randomR (-1000, 1000) r1
