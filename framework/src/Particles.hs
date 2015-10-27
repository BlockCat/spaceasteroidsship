{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Particles where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point 
import Graphics.Gloss
import RandomUtils

    
data Particle = Particle {
        -- Event queue
        location        :: Point,
        speed           :: Vector,
        lifetime        :: Float,
        drawing         :: Picture
    } deriving Show
                
createParticle :: Point -> Vector -> Float -> Picture -> Particle
createParticle loc spd life pict = Particle loc spd life pict
            
randomizedParticle :: Float -> Float -> Float -> Particle -> StdGen -> (Particle, StdGen)
randomizedParticle spdVar degVar lifeVar particle@(Particle {..}) rndGen = (particle{speed = newSpeed, lifetime = lifetime + lifeVariation}, r3)    
    where
    (speedVariation, r1) = randomR (negate spdVar,  spdVar)  rndGen
    (degreVariation, r2) = randomR (negate degVar,  degVar)  r1
    (lifeVariation , r3) = randomR (negate lifeVar, lifeVar) r2
    newSpeed             = rotateV (degreVariation * pi / 180) (speed + (speedVariation, 0))
    
    
explosion :: Point -> StdGen -> ([Particle], StdGen)
explosion loc rndGen = (outerExplosion ++ middleExplosion ++ innerExplosion , r3)
    where
        speedVar                   = 16
        particleCreator speed pict = randomizedParticle speedVar 180 5 (createParticle loc (speed, 0) 4 pict)
        (innerExplosion,  r1)      = generateRandom rndGen (particleCreator 20 $ color yellow $ circleSolid 1) 1000
        (outerExplosion,  r2)      = generateRandom r1 (particleCreator 30 $ color red    $ circleSolid 1) 1000
        (middleExplosion, r3)      = generateRandom r1 (particleCreator 25 $ color orange    $ circleSolid 1) 1000
    
updateParticles :: Float -> [Particle] -> [Particle]
updateParticles elapsed xs = filter (\particle -> lifetime particle > 0) (map (updateParticle elapsed) xs) 

updateParticle  :: Float -> Particle -> Particle
updateParticle elapsed particle@(Particle{..}) = particle {lifetime = lifetime - elapsed, location = location + (mulSV elapsed speed)}

drawParticles :: [Particle] -> Picture
drawParticles xs = Pictures $ map drawParticle xs

drawParticle :: Particle -> Picture
drawParticle (Particle{..}) = translate x y drawing
    where
    x = fst location
    y = snd location