{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
import Graphics.Gloss.Data.Point

import Model
import Player
import Stars
import Particles
import Bullets
import Enemies
import Multiplier

-- | Drawing

horizontalResolution = 1200.0
verticalResolution = 600.0


draw :: Float -> Float -> World -> Picture
draw horizontalResolution' verticalResolution' world@(World{..})
    = Pictures [relativePict, staticPict]

    where
        relativePict  = (translate cameraOffsetX cameraOffsetY . Pictures) [stars', boundary', particles', player', enemies', bullets', multipliers']
        staticPict    = Pictures [score']
        stars'        = drawStars     player starField 
        particles'    = drawParticles particles
        player'       = drawPlayer    player
        bullets'      = drawBullets   bullets
        multipliers'  = drawMultipliers multipliers
        enemies'      = drawEnemies   enemies enemyImage
        boundary'     = color blue $ rectangleWire 2000 2000
        cameraOffsetX = (negate . fst . playerLocation) player
        cameraOffsetY = (negate . snd . playerLocation) player
        score'        = drawScore playerScore
    
drawStars :: Player -> [Star] -> Picture
drawStars player = Pictures . map (drawStar player)

drawStar :: Player -> Star -> Picture
drawStar Player{..} Star{..} | shouldDraw = translate nx ny picture 
                             | otherwise  = blank
    where
    (px, py)   = playerLocation
    (sx, sy)   = starLocation
    (nx, ny)   = (sx - px/starDepth^2, sy - py/starDepth^2)    
    picture    = (color white . circleSolid) (4 - starDepth)
    width      = 600
    height     = 300
    p1         = (px - width, py - height)
    p2         = (px + width, py + height)
    shouldDraw = pointInBox (nx, ny) p1 p2
  
drawPlayer :: Player -> Picture
drawPlayer player@(Player{..}) = (translate x y . rotate (90-direction) . scale 2 2) playerImage
    where 
        (x, y) = playerLocation

drawScore  :: Int -> Picture
drawScore score = (translate (-405) (250) . scale 0.2 0.2 . color yellow . text) ("Score: " ++ (show score))
