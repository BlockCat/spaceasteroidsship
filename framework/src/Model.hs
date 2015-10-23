{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Data.Point
import Stars

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
        bullets          :: [Bullet],
        starField        :: [Star]
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
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot

initial :: Int -> IO World
initial seed =
    do
    stars <- generateStarField rndGen 1000
    return $ World rndGen NoRotation NoMovement DontShoot player [] stars
    where     
    player = Player {playerLocation = (100, 100), playerSpeed = (0, 0), direction = 0}
    rndGen = mkStdGen seed
