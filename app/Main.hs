{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Gloss

window :: Display
--- Window with name size and position
window = InWindow "Nice window?" (600, 800) (100, 100)

bgColour :: Color
bgColour = white

drawing :: Picture
drawing = circle 80

main :: IO ()
main = display window bgColour drawing
