module Main where

import Controller
import Model
import View


import Graphics.Gloss.Interface.IO.Game


main :: IO ()
main = playIO (InWindow "window" (800,450) (0, 0)) -- Or FullScreen
              black            -- Background color
              fps               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function