{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns #-}

module Model where

import System.Random

-- | Game state

data World = World {
        -- Random generator
        rndGen           :: StdGen,
        -- Event queue
        rotateAction     :: RotateAction,
        movementAction   :: MovementAction,
        shootAction      :: ShootAction,
        -- TODO: add more fields here!
        player          :: Player
    }
data Player = Player {
        --Player location
        x               :: Float,
        y               :: Float,
        dx              :: Float,        
        dy              :: Float,
        direction       :: Float
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust
data ShootAction    = Shoot      | DontShoot

initial :: Int -> World
initial seed = World (mkStdGen seed) NoRotation NoMovement DontShoot player
    where 
    player = Player {x = 100, y = 100, dx = 0, dy = 0, direction = 0}
