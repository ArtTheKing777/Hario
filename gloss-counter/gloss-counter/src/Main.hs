module Main where

import Controller
import Model
import View

import Animations
import Fileload

import Graphics.Gloss.Interface.IO.Game
import Fileload (getAcidBmp)


import Graphics.Gloss (loadBMP)
import Data.Map


main :: IO ()
main = do
    --get all the frames and load them once here because haskell
    harioBmp <- getHarioBmp
    harioAnimationSheetBmp <- getHarioAnimationSheetBmp


    let loadedAnimations = fromList [ ("harioBmp",harioBmp),("harioAnimationSheetBmp",harioAnimationSheetBmp)]

    playIO (InWindow "window" (800,450) (0, 0)) -- Or FullScreen
              (makeColorI 135 206 235 255)          -- Background color
              fps               -- Frames per second
              (initialState loadedAnimations)     -- Initial state
              view             -- View function
              input            -- Event function
              update_           -- update function