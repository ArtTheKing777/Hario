module Main where

import Controller
import Model
import View

import Animations

import Graphics.Gloss.Interface.IO.Game
import Fileload (getAcidBmp)


import Graphics.Gloss (loadBMP)
import Data.Map


main :: IO ()
main = do
    --get all the frames and load them once
    harioFrames <- getHarioFrames
    harioFireFrames <- getFireHarioFrames
    smallHarioFrames <- getSmallHarioFrames
    henemyFrames <- getHenemyFrames
    hammerFrames <- getHammerFrames
    hireBallFrames <- getHireBallFrames 
    acidFrames <- getAcidFrames 
    wormFrames <- getWormFrames
    howserFrames <- getHowserFrames

    let loadedAnimations = fromList [("harioFrames",harioFrames),("harioFireFrames",harioFireFrames)]

    playIO (InWindow "window" (800,450) (0, 0)) -- Or FullScreen
              (makeColorI 135 206 235 255)          -- Background color
              fps               -- Frames per second
              (initialState loadedAnimations)     -- Initial state
              view             -- View function
              input            -- Event function
              update_           -- update function