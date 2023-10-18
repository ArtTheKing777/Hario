-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss ( loadBMP, Picture (Bitmap, BitmapSection) , pictures, scale, translate, BitmapData (bitmapSize), bitmapDataOfBMP, bitmapSection, Rectangle (Rectangle) )
import Model ( GameState (LevelSelectState) )
import Graphics.Gloss.Interface.IO.Animate (animateIO)

view :: GameState -> IO Picture
view g@(LevelSelectState k eT)  = do
                hario <- getHario
                hario <- pure (scale 0.5 0.5 hario)
                hario <- pure (translate (-400) 0 hario)

                hario2 <- getHario
                hario2 <- pure (scale 0.5 0.5 hario2)
                hario2 <- pure (translate 400 0 hario2)

                harioAnimationsheet <- getHarioAnimationSheet

                let harioAnimationSprites = (makeListofSheet (Rectangle (0, 0) (17, -35)) harioAnimationsheet)

                return (pictures ([hario, hario2]++harioAnimationSprites))

getHario :: IO Picture
getHario = loadBMP "media/Super_Hario_Bros_Logo.bmp"

getAnimationSheet1 :: IO Picture
getAnimationSheet1 = loadBMP "media/hario_and_items.bmp"

toBitmapData :: Picture -> BitmapData
toBitmapData p@(Bitmap bitmap) = bitmap

getHarioAnimationSheet :: IO Picture
getHarioAnimationSheet = do
                            harioSheet <- getAnimationSheet1
                            let harioSheetbmp = (toBitmapData harioSheet)
                            return (bitmapSection (Rectangle (0, 0) (270, -35)) harioSheetbmp )

getFireHarioAnimationSheet :: IO Picture
getFireHarioAnimationSheet = do
                                fireHarioSheet <- getAnimationSheet1
                                let fireHarioSheetbmp = (toBitmapData fireHarioSheet)
                                return (bitmapSection (Rectangle (0, -35) (360, -71)) fireHarioSheetbmp)

getSmallHarioAnimationSheet :: IO Picture
getSmallHarioAnimationSheet = do
                                smallHarioSheet <- getAnimationSheet1
                                let smallHarioSheetbmp = (toBitmapData smallHarioSheet)
                                return (bitmapSection (Rectangle (0, -71) (251, -89)) smallHarioSheetbmp)

makeListofSheet :: Rectangle -> Picture -> [Picture]
makeListofSheet r@(Rectangle (x,y) (w,h)) bmp   | fst (bitmapSize bitmp) <= w  = [bmp]
                                                | otherwise = BitmapSection r bitmp : makeListofSheet r (BitmapSection (Rectangle (x+w, y) (wd, h)) bitmp)
                                                where
                                                    wd = fst (bitmapSize (toBitmapData bmp)) - w
                                                    bitmp = toBitmapData bmp


-- | 270x-35px for normal hario
-- | 360x-71px from (0, -35) for fire hario
-- | 251x-89px from (0, -71) for small hario

-- | normal hario = 17x35px per sprite, fire hario is 1px taller
-- | small hario = 15x18px per sprite 