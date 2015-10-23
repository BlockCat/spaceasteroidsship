{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import Graphics.Gloss
import Stars
import Particles

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        -- TODO: add more fields here!
        player          :: Player,
        starField       :: [Star],
        particles       :: [Particle],
        bullets          :: [Bullet]

    }
    
data Player = Player {
        --Player location
        playerLocation  :: Point,
        playerSpeed     :: Vector,
        direction       :: Float
    }

    
data Bullet = Bullet {
        bulletX         :: Float,
        bulletY         :: Float,
        bulletDir       :: Float
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust deriving Eq
data ShootAction    = Shoot      | DontShoot

initial :: Int -> World
initial seed = World rndGen NoRotation NoMovement DontShoot player stars [] []
    where         
    r1 = mkStdGen seed
    (stars, rndGen) = generateStarField r1 1000 
    player = Player {playerLocation = (100, 100), playerSpeed = (0, 0), direction = 0}
    
