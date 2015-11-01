{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Multiplier where

import Graphics.Gloss
import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Geometry.Angle
import Player
import Location
import RandomUtils

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
spawnMultiplierAtRandomLocation stdGen = (Multiplier loc, newStdGen)
    where
        (loc, newStdGen) = randomLocation stdGen
