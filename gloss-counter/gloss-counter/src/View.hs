-- | This module defines how to turn
--   the game state into a picture
module View where
import Graphics.Gloss.Data.Picture (Picture (Bitmap, BitmapSection, Blank) , pictures, scale, translate, bitmapSection)
import Model
import Graphics.Gloss.Interface.IO.Animate (animateIO)
import Graphics.Gloss.Data.Bitmap (BitmapData(bitmapSize), Rectangle (Rectangle), loadBMP)
import Data.Fixed
import GHC.Float (int2Float)

view :: GameState -> IO Picture
view g@(StartScreenState k t)  = testShow t
view g@(LevelSelectState k t)  = testShow t
view g@(LevelPlayingState k t) = testShow t

testShow :: Float -> IO Picture
testShow t = do
                hario <- getHario
                hario <- pure (scale 0.5 0.5 (Bitmap hario))
                hario <- pure (translate (-400) 0 hario)

                hario2 <- getHario
                hario2 <- pure (scale 0.5 0.5 (Bitmap hario2))
                hario2 <- pure (translate 400 0 hario2)
                
                harioSheetBmp <- getHarioAnimationSheet

                let harioSheet = makeListofSheet (Rectangle (0, -1) (17, -35)) harioSheetBmp (fst (bitmapSize harioSheetBmp))

                let hario1 = harioSheet !! 2

                return (animationLoop t (0.25) harioSheet)

getHario :: IO BitmapData
getHario = loadBitmapData "media/Super_Hario_Bros_Logo.bmp"

loadBitmapData :: FilePath -> IO BitmapData
loadBitmapData s = do
                        p@(Bitmap bmpdata) <- loadBMP s
                        return bmpdata

getHarioAnimationSheet :: IO BitmapData
getHarioAnimationSheet = loadBitmapData "media/hario.bmp"

getFireHarioAnimationSheet :: IO BitmapData
getFireHarioAnimationSheet = loadBitmapData "media/firehario.bmp"

getSmallHarioAnimationSheet :: IO BitmapData
getSmallHarioAnimationSheet = loadBitmapData "media/smallhario.bmp"

makeListofSheet :: Rectangle -> BitmapData -> Int -> [Picture]
makeListofSheet r@(Rectangle (x,y) (w,h)) bmp l | l >= w  = BitmapSection r bmp : makeListofSheet (Rectangle (x+w+1, y) (w,h)) bmp (l-w)
                                                | otherwise = []

-- time, time per frame, pictures
animationLoop :: Float -> Float -> [Picture] -> Picture
animationLoop eT tF p = p !! round(mod' eT ((tF * int2Float(length p))-1))


-- | 270x-35px for normal hario
-- | 360x-71px from (0, -35) for fire hario
-- | 251x-89px from (0, -71) for small hario

-- | normal hario = 17x35px per sprite, fire hario is 1px taller
-- | small hario = 15x18px per sprite 