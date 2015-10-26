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

-- | Drawing

horizontalResolution = 1200.0
verticalResolution = 600.0


draw :: Float -> Float -> World -> Picture
draw horizontalResolution' verticalResolution' world@(World{..})
    = translate cameraOffsetX cameraOffsetY $ scale scaledX scaledY $ Pictures [stars', boundary', particles', player', enemies', bullets', viewPort]
    where
        stars'        = drawStars     player starField 
        particles'    = drawParticles particles
        player'       = drawPlayer    player
        bullets'      = drawBullets   bullets
        enemies'      = drawEnemies   enemies
        viewPort      = translate (negate cameraOffsetX) (negate cameraOffsetY) $ color red $ rectangleWire 1000 700
        boundary'     = color blue $ rectangleWire 2000 2000
        cameraOffsetX = (negate . fst . playerLocation) player
        cameraOffsetY = (negate . snd . playerLocation) player
        scaledX       = horizontalResolution / horizontalResolution'
        scaledY       = verticalResolution   / verticalResolution'
    
drawStars :: Player -> [Star] -> Picture
drawStars player xs = Pictures $ map (drawStar player) xs

drawStar :: Player -> Star -> Picture
drawStar (Player{playerLocation}) (Star{location, depth}) = if shouldDraw then translate nx ny picture else blank
    where
    px = fst playerLocation
    py = snd playerLocation
    sx = fst location
    sy = snd location
    nx = sx - px/(depth*depth)
    ny = sy - py/(depth*depth)
    
    picture = color white (circleSolid (4 - depth))
    (cameraOffsetX , cameraOffsetY) = playerLocation
    width         = 600
    height        = 300
    p1            = (cameraOffsetX - width, cameraOffsetY - height)
    p2            = (cameraOffsetX + width, cameraOffsetY + height)
    shouldDraw    = pointInBox (nx, ny) p1 p2
    

drawPlayer :: Player -> Picture
drawPlayer player@(Player {playerLocation, direction}) = (translate x y . scale 0.6 0.6 . pictures) [color red fillTriangleL, color red fillTriangleR,color white bodyTriangle1, color blue bodyTriangle2, color red lowerShootTriangleR, 
                  color red lowerShootTriangleL, color red shootRectR, color red shootRectL, color red upperShootTriangleR, color red upperShootTriangleL, color grey lowerShootTriangleR2, color grey lowerShootTriangleL2, color grey upperShootTriangleR2, color grey upperShootTriangleL2]
    where 
    fillTriangleR        = rotate (90-direction :: Float)  $ polygon       [(35, -20), (40, -20), (35, -10)]
    fillTriangleL        = rotate (90-direction :: Float)  $ polygon       [(-40, -20), (-35, -20), (-35, -10)]
    bodyTriangle1        = rotate (90-direction :: Float)  $ polygon       [(-40, -20), (40, -20), (0, 20)]
    bodyTriangle2        = rotate (90-direction :: Float)  $ polygon       [(-25, -15), (25, -15), (0, 10)]
    lowerShootTriangleR  = rotate (90-direction :: Float)  $ polygon       [(35, -10), (45, -10), (40, -20)]
    lowerShootTriangleL  = rotate (90-direction :: Float)  $ polygon       [(-45, -10), (-35, -10), (-40, -20)]
    shootRectR           = rotate (90-direction :: Float)  $ drawRectangle 40 (-5) 10 10
    shootRectL           = rotate (90-direction :: Float)  $ drawRectangle (-40) (-5) 10 10 
    upperShootTriangleR  = rotate (90-direction :: Float)  $ polygon       [(35, 0), (45, 0), (40, 20)]
    upperShootTriangleL  = rotate (90-direction :: Float)  $ polygon       [(-45, 0), (-35, 0), (-40, 20)]
    lowerShootTriangleR2 = rotate (90-direction :: Float)  $ polygon       [(37, -3), (43, -3), (40, -18)]
    lowerShootTriangleL2 = rotate (90-direction :: Float)  $ polygon       [(-43, -3), (-37, -3), (-40, -18)]
    upperShootTriangleR2 = rotate (90-direction :: Float)  $ polygon       [(37, -3), (43, -3), (40, 15)]
    upperShootTriangleL2 = rotate (90-direction :: Float)  $ polygon       [(-43, -3), (-37, -3), (-40, 15)]
    
    grey                 = greyN 0.4
    (x, y)               = playerLocation
    
-- Draws a rectangle with the x and y coordinate of middle of rectangle + the width and height 
drawRectangle :: Float -> Float -> Float -> Float -> Picture
drawRectangle x y width = translate x y . rectangleSolid width
