module Animations where
import Fileload
import Graphics.Gloss.Data.Bitmap (Rectangle (Rectangle), BitmapData (bitmapSize))
import Graphics.Gloss.Data.Picture (Picture (BitmapSection))
import Data.Fixed (mod')
import GHC.Float (int2Float)

makeListofSheet :: Rectangle -> BitmapData -> Int -> [Picture]
makeListofSheet r@(Rectangle (x,y) (w,h)) bmp l | l >= w  = BitmapSection r bmp : makeListofSheet (Rectangle (x+w+1, y) (w,h)) bmp (l-w)
                                                | otherwise = []

-- time, time per frame, pictures
animationLoop :: Float -> Float -> [Picture] -> Picture
animationLoop eT tF p = p !! floor (mod' (eT/tF) (int2Float (length p)))

getHarioFrames :: IO [Picture]
getHarioFrames = do
                    harioSheetBmp <- getHarioAnimationSheet
                    return (makeListofSheet (Rectangle (0, -1) (17, -35)) harioSheetBmp (fst (bitmapSize harioSheetBmp)))

harioWalk :: IO [Picture]
harioWalk = do
                harioFrames <- getHarioFrames
                return [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]


-- | 270x-35px for normal hario
-- | 360x-71px from (0, -35) for fire hario
-- | 251x-89px from (0, -71) for small hario

-- | normal hario = 17x35px per sprite, fire hario is 1px taller
-- | small hario = 15x18px per sprite 