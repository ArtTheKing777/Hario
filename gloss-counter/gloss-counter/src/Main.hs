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
import System.Environment ( getArgs, getArgs )
import System.Directory (renameFile, findFile)
import System.Random


main :: IO ()
main = do
    file <- findFile ["media"] "HarioSaveTmp.txt"
    let deltmp = case file of
            Nothing -> return()
            Just a -> renameFile a "media/HarioSave.txt"
    doit <- deltmp

    --get all the frames and load them once here because haskell
    harioBmp <- getHarioBmp
    harioAnimationSheetBmp <- getHarioAnimationSheetBmp
    fireHarioAnimationSheetBmp <- getFireHarioAnimationSheetBmp
    smallHarioAnimationSheetBmp <- getSmallHarioAnimationSheetBmp
    textBocBmp <- getTextBoxBmp
    textBocBmpNo <- getTextBoxBmpNo
    henemiesBmp <- getHenemiesBmp
    howserBmp <- getHowserBmp
    hammerBmp <- getHammerBmp
    fireBmp <- getFireBmp
    fireballBmp <- getFireBallBmp
    wormBmp <- getWormBmp
    acidBmp <- getAcidBmp
    fireFlower <- getFireFlowerBmp
    mushroom <- getMushRoomBmp
    tilesBmp <- getTilesBmp
    pipeBmp <- getPipeBmp
    coinsBmp <- getCoinsBmp
    flagBmp <- getFlagBmp
    hoolitBillTowerBmp <- getHoolitBillTowerBmp
    level0 <- getSave "harioSave"
    level1 <- getLevel "1"
    level2 <- getLevel "2"
    seed <- newStdGen

    -- put it in a dictonary 
    let loadedAnimations = fromList [ ("harioBmp",harioBmp),
            ("harioAnimationSheetBmp",harioAnimationSheetBmp),
            ("smallHarioAnimationSheetBmp",smallHarioAnimationSheetBmp),
            ("fireHarioAnimationSheetBmp",fireHarioAnimationSheetBmp),
            ("textBoxBmp",textBocBmp),
            ("textBoxBmpNo",textBocBmpNo),
            ("henemiesBmp",henemiesBmp),
            ("howserBmp", howserBmp),
            ("hammerBmp",hammerBmp),
            ("fireBmp",fireBmp),
            ("fireballBmp", fireballBmp),
            ("wormBmpMove",head wormBmp),
            ("wormBmpSpit",wormBmp!!1),
            ("wormBmpCharge",wormBmp!!2),
            ("acidBmpMove",head acidBmp),
            ("acidBmpSplat",acidBmp!!1),
            ("Mushroom",mushroom),
            ("Fireflower",fireFlower),
            ("tilesBmp1",head tilesBmp),
            ("tilesBmp2",tilesBmp!!1),
            ("pipeBmp",pipeBmp),
            ("coinsBmp",coinsBmp),
            ("flagBmp",flagBmp),
            ("hoolitBillTowerBmp",hoolitBillTowerBmp)]
        rSeed = randomR (1::Int,10000::Int) seed :: (Int,StdGen)
        loadedLevels = [level0,level1,level2,[show $ fst rSeed ]]

    playIO (InWindow "Hario" (800,450) (0, 0)) -- Or FullScreen
              (makeColorI 135 206 235 255)          -- Background color
              fps               -- Frames per second
              (initialState loadedAnimations loadedLevels)     -- Initial state
              view             -- View function
              input            -- Event function
              update_           -- update function