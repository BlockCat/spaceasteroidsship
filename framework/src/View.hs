{-# LANGUAGE RecordWildCards #-}

module View (
    draw
) where

import Graphics.Gloss
--import Graphics.Gloss.Geometry

import Model

-- | Drawing

draw :: Float -> Float -> World -> Picture
draw horizontalResolution verticalResolution world@(World{player, ..})
    = Pictures [(drawPlayer player)]
    
drawPlayer :: Player -> Picture
drawPlayer player@(Player {x, y, direction})= translate (x) (y) (color red triangle)
    where 
    triangle = rotate (90-direction :: Float) $ polygon  [(-10, -20), (10, -20), (0, 40)]