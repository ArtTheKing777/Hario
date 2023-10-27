module Main where

import Controller
import Model
import View


import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = playIO (InWindow "window" (800,450) (0, 0)) -- Or FullScreen
              (makeColorI 135 206 235 255)          -- Background color
              fps               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              update           -- update function