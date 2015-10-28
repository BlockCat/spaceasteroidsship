{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns#-}

module RandomUtils where

import System.Random
import Control.Monad  (replicateM)
import Graphics.Gloss (Vector, Point)
                
generateRandom :: StdGen -> (StdGen -> (a, StdGen)) -> Int -> ([a], StdGen)
generateRandom rndGen f n = createReturn list
    where
        list         = foldr (\x acc -> (x . snd . head) acc : acc) [f rndGen] $ replicate (n-1) f
        createReturn []         = ([], rndGen)
        createReturn [(s, r)]   = ([s], r)
        createReturn ((s, r):xs)= (s:s', r)
            where 
                (s', r') = createReturn xs
