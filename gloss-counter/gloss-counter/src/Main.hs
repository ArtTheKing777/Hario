module Main where

import Controller
import Model
import View

import Animations

import Graphics.Gloss.Interface.IO.Game
import Fileload (getAcidBmp, getHarioBmp, getHarioAnimationSheetBmp)


import Graphics.Gloss (loadBMP)
import Data.Map


main :: IO ()
main = do
    --get all the frames and load them once
    harioBmp <- getHarioBmp
    harioAnimationSheetBmp <- getHarioAnimationSheetBmp

    let loadedAnimations = fromList [("harioFrames",harioBmp),("harioAnimationSheetBmp", harioAnimationSheetBmp)]

    playIO (InWindow "window" (800,450) (0, 0)) -- Or FullScreen
              (makeColorI 135 206 235 255)          -- Background color
              fps               -- Frames per second
              (initialState loadedAnimations)     -- Initial state
              view             -- View function
              input            -- Event function
              update_           -- update function