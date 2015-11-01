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
import Multiplier

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
        multipliers      :: [Multiplier],
        enemies          :: [Enemy],
        enemySpawnTimer  :: Float,
        multiplierTimer  :: Float,
        enemyImage       :: Picture,
        multiplierImage  :: Picture,
        playerScore      :: Int,
        scoreMultiplier  :: Int
    }
    
data RotateAction   = NoRotation | RotateLeft | RotateRight
data MovementAction = NoMovement | Thrust deriving Eq
data ShootAction    = Shoot      | DontShoot

initial :: Int -> Picture -> Picture -> Picture -> World
initial seed playerPng enemyPng multiplierPng = World {
                        rndGen          = rndGen,   
                        rotateAction    = NoRotation,
                        movementAction  = NoMovement,
                        shootAction     = DontShoot,
                        player          = player,
                        starField       = stars,
                        particles       = [],
                        bullets         = [],
                        multipliers     = [],
                        enemies         = [],
                        enemySpawnTimer = 4,
                        multiplierTimer = 6,
                        enemyImage      = enemyPng,
                        multiplierImage = multiplierPng,
                        playerScore     = 0,
                        scoreMultiplier = 1
                     }
    where         
    (stars, rndGen) = generateStarField (mkStdGen seed) 3000 
    player          = Player (100, 100) (0, 0) 0 playerPng 0 True
    
emptyWorld :: World -> World
emptyWorld world@(World{..})= 
        world {
                rotateAction    = NoRotation,
                movementAction  = NoMovement,
                shootAction     = DontShoot,
                player          = player',
                bullets         = [],
                multipliers     = [],
                enemies         = [],                
                enemySpawnTimer = 4,
                playerScore     = 0,
                scoreMultiplier      = 1
             }
    where
        player' = player {playerLocation = (100, 100), playerSpeed = (0, 0)}
