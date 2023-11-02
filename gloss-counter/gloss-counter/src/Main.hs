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
    fireHarioAnimationSheetBmp <- getFireHarioAnimationSheetBmp
    smallHarioAnimationSheetBmp <- getSmallHarioAnimationSheetBmp
    textBocBmp <- getTextBoxBmp
    henemiesBmp <- getHenemiesBmp
    howserBmp <- getHowserBmp
    hammerBmp <- getHammerBmp
    fireBmp <- getFireBmp
    wormBmp <- getWormBmp
    acidBmp <- getAcidBmp
    tilesBmp <- getTilesBmp
    pipeBmp <- getPipeBmp
    coinsBmp <- getCoinsBmp
    flagBmp <- getFlagBmp
    hoolitBillTowerBmp <- getHoolitBillTowerBmp
    level1 <- getLevel "1"
    level2 <- getLevel "2"

    -- put it in a dictonary 
    let loadedAnimations = fromList [ ("harioBmp",harioBmp),
            ("harioAnimationSheetBmp",harioAnimationSheetBmp),
            ("smallHarioAnimationSheetBmp",smallHarioAnimationSheetBmp),
            ("fireHarioAnimationSheetBmp",fireHarioAnimationSheetBmp),
            ("textBoxBmp",textBocBmp),
            ("henemiesBmp",henemiesBmp),
            ("howserBmp", howserBmp),
            ("hammerBmp",hammerBmp),
            ("fireBmp",fireBmp),
            ("wormBmpMove",head wormBmp),
            ("wormBmpSpit",wormBmp!!1),
            ("wormBmpCharge",wormBmp!!2),
            ("acidBmpMove",head acidBmp),
            ("acidBmpSplat",acidBmp!!1),
            ("tilesBmp1",head tilesBmp),
            ("tilesBmp2",tilesBmp!!1),
            ("pipeBmp",pipeBmp),
            ("coinsBmp",coinsBmp),
            ("flagBmp",flagBmp),
            ("hoolitBillTowerBmp",hoolitBillTowerBmp)]
        loadedLevels = [level1,level2]

    playIO (InWindow "Hario" (800,450) (0, 0)) -- Or FullScreen
              (makeColorI 135 206 235 255)          -- Background color
              fps               -- Frames per second
              (initialState loadedAnimations loadedLevels)     -- Initial state
              view             -- View function
              input            -- Event function
              update_           -- update function