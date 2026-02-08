{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Gloss

--- Window with name size and position
window :: Display
window = InWindow "Nice window?" (600, 800) (100, 100)

bgColour :: Color
bgColour = black

drawing :: Picture
drawing = color white $ thickCircle 80 5

main :: IO ()
main = display window bgColour drawing
