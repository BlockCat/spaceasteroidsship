{-# LANGUAGE DisambiguateRecordFields, NamedFieldPuns, RecordWildCards #-}

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
        enemies          :: [Enemy],
        enemySpawnTimer       :: Float,
        enemyImage       :: Picture
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust deriving Eq
data ShootAction    = Shoot      | DontShoot

initial :: Int -> Picture -> Picture -> World
initial seed playerBmp enemyBmp = World {
                        rndGen         = rndGen,
                        rotateAction   = NoRotation,
                        movementAction = NoMovement,
                        shootAction    = DontShoot,
                        player         = player,
                        starField      = stars,
                        particles      = [],
                        bullets        = [],
                        enemies        = [],
                        enemySpawnTimer= 4,
                        enemyImage     = enemyBmp
                     }
    where         
    r1 = mkStdGen seed
    (stars, rndGen) = generateStarField r1 3000 
    player = Player (100, 100) (0, 0) 0 playerBmp
    
emptyWorld :: World -> World
emptyWorld world@(World{..})= 
        world {
                rotateAction   = NoRotation,
                movementAction = NoMovement,
                shootAction    = DontShoot,
                player         = player',
                bullets        = [],
                enemies        = [],
                enemySpawnTimer     = 4
             }
    where
        player' = player {playerLocation = (100, 100), playerSpeed = (0, 0)}
