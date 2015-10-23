{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

module Particles where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point 
import Graphics.Gloss

    
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
    
    
updateParticles :: Float -> [Particle] -> [Particle]
updateParticles elapsed xs = filter (\particle -> lifetime particle > 0) (map (updateParticle elapsed) xs) 

updateParticle  :: Float -> Particle -> Particle
updateParticle elapsed particle@(Particle{..}) = particle {lifetime = lifetime - elapsed, location = location + speed}

drawParticles :: [Particle] -> Picture
drawParticles xs = Pictures $ map drawParticle xs

drawParticle :: Particle -> Picture
drawParticle (Particle{..}) = translate x y drawing
    where
    x = fst location
    y = snd location