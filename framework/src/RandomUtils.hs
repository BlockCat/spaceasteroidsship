{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns#-}

module RandomUtils where

import System.Random
import Control.Monad  (replicateM)
import Graphics.Gloss (Vector, Point)
                
generateRandom :: StdGen -> (StdGen -> (a, StdGen)) -> Int -> ([a], StdGen)
generateRandom rndGen f n = last $ take n $ iterate next ([], rndGen)
    where
        next (xs, r) = (x':xs, r')
            where
                (x', r') = f r
                
                
randomLocation :: StdGen -> ((Float, Float), StdGen)
randomLocation stdGen = ((x',y'), r2)
    where
        (x', r1) = randomR (-1000, 1000) stdGen 
        (y', r2) = randomR (-1000, 1000) r1

