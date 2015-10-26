{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import Graphics.Gloss

import Player
import Stars
import Particles
import Bullets
import Enemies

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        -- TODO: add more fields here!
        player           :: Player,
        starField        :: [Star],
        particles        :: [Particle],
        bullets          :: [Bullet],
        enemies          :: [Enemy]
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust deriving Eq
data ShootAction    = Shoot      | DontShoot

initial :: Int -> World
initial seed = World rndGen NoRotation NoMovement DontShoot player stars [] [] [enemy]
    where         
    r1 = mkStdGen seed
    (stars, rndGen) = generateStarField r1 3000 
    player = Player (100, 100) (0, 0) 0
    enemy  = Enemy (50, 150) (0,0) 0
    
