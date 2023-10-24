module Animations where
import Fileload
import Graphics.Gloss.Data.Bitmap (Rectangle (Rectangle), BitmapData (bitmapSize))
import Graphics.Gloss.Data.Picture (Picture (BitmapSection))
import Data.Fixed (mod')
import GHC.Float (int2Float)
import Model (PlayerPower (Small, Big, Fire))

-- rectangle for first sprite, bitmapdata of image
makeListofSheet :: Rectangle -> BitmapData -> Int -> [Picture]
makeListofSheet r@(Rectangle (x,y) (w,h)) bmp l | l >= w  = BitmapSection r bmp : makeListofSheet (Rectangle (x+w+1, y) (w,h)) bmp (l-w)
                                                | otherwise = []

-- time, time per frame, pictures
animationLoop :: Float -> Float -> [Picture] -> Picture
animationLoop eT tF p = p !! floor (mod' (eT/tF) (int2Float (length p)))

--load frames as [Pictures]
getHarioFrames :: IO [Picture]
getHarioFrames = do
                    harioSheetBmp <- getHarioAnimationSheet
                    return (makeListofSheet (Rectangle (0, -1) (17, -35)) harioSheetBmp (fst (bitmapSize harioSheetBmp)))
getFireHarioFrames :: IO [Picture]
getFireHarioFrames = do
                        fireHarioSheetBmp <- getFireHarioAnimationSheet
                        return (makeListofSheet (Rectangle (0, -36) (17, -36)) fireHarioSheetBmp (fst (bitmapSize fireHarioSheetBmp)))
getSmallHarioFrames :: IO [Picture]
getSmallHarioFrames = do
                        smallHarioSheetBmp <- getSmallHarioAnimationSheet
                        return (makeListofSheet (Rectangle (0, -72) (17, -18)) smallHarioSheetBmp (fst (bitmapSize smallHarioSheetBmp)))
-- animation sheets
idleSheet :: PlayerPower -> IO [Picture]
idleSheet Small = do
                    harioFrames <- getSmallHarioFrames
                    return [head harioFrames]
idleSheet Big = do
                    harioFrames <- getHarioFrames
                    return [head harioFrames]
idleSheet Fire = do
                    harioFrames <- getFireHarioFrames
                    return [head harioFrames]

jumpSheet :: PlayerPower -> IO [Picture]
jumpSheet Small = do
                    harioFrames <- getSmallHarioFrames
                    return [harioFrames !! 1, harioFrames !! 2]
jumpSheet Big = do
                    harioFrames <- getHarioFrames
                    return [harioFrames !! 1, harioFrames !! 2]
jumpSheet Fire = do 
                    harioFrames <- getFireHarioFrames
                    return [harioFrames !! 1, harioFrames !! 2]

squatSheet :: PlayerPower -> IO [Picture]
squatSheet Small = do
                    harioFrames <- getSmallHarioFrames
                    return [harioFrames !! 3]
squatSheet Big = do
                    harioFrames <- getHarioFrames
                    return [harioFrames !! 3]
squatSheet Fire = do
                    harioFrames <- getFireHarioFrames
                    return [harioFrames !! 3]

walkSheet :: PlayerPower -> IO [Picture]
walkSheet Small = do
                        harioFrames <- getSmallHarioFrames
                        return [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]
walkSheet Big = do
                        harioFrames <- getHarioFrames
                        return [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]
walkSheet Fire = do
                        harioFrames <- getFireHarioFrames
                        return [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]

swimSheet :: PlayerPower -> IO [Picture]
swimSheet Small = do
                    harioFrames <- getSmallHarioFrames
                    return [harioFrames !! 8, harioFrames !! 9, harioFrames !! 10, harioFrames !! 12, harioFrames !! 13, harioFrames !! 14, harioFrames !! 7]
swimSheet Big = do
                    harioFrames <- getHarioFrames
                    return [harioFrames !! 8, harioFrames !! 9, harioFrames !! 10, harioFrames !! 12, harioFrames !! 13, harioFrames !! 14, harioFrames !! 7]
swimSheet Fire = do
                    harioFrames <- getHarioFrames
                    return [harioFrames !! 8, harioFrames !! 9, harioFrames !! 10, harioFrames !! 12, harioFrames !! 13, harioFrames !! 14, harioFrames !! 7]
-- fire hario only
fireShootSheet :: IO [Picture]
fireShootSheet = do 
                    harioFrames <- getFireHarioFrames
                    return [harioFrames !! 15]

fireShootMoveSheet :: IO [Picture]
fireShootMoveSheet = do
                    harioFrames <- getFireHarioFrames
                    return [harioFrames !! 1, harioFrames !! 15, harioFrames !! 18, harioFrames !! 19, harioFrames !! 16, harioFrames !! 17, harioFrames !! 18]




-- | 270x-35px for normal hario
-- | 360x-71px from (0, -35) for fire hario
-- | 251x-89px from (0, -71) for small hario

-- | normal hario = 17x35px per sprite, fire hario is 1px taller
-- | small hario = 15x18px per sprite 