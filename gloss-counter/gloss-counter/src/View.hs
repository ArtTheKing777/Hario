-- | This module defines how to turn
--   the game state into a picture
module View where
import Graphics.Gloss.Data.Picture (Picture (Bitmap, BitmapSection, Blank) , pictures, scale, translate, bitmapSection, bitmap)
import Model
import Animations
import Graphics.Gloss.Interface.IO.Animate (animateIO)
import Data.Fixed
import GHC.Float (int2Float)
import Graphics.Gloss

fps :: Int
fps = 60

harioSpeed :: Float
harioSpeed = 10

view :: GameState -> IO Picture
view g@(StartScreenState k t mp)  = return(scale 0.1 0.1 (color blue (text (show(fst mp) ++ " " ++ show(snd mp)))))
view g@(LevelSelectState k t)  = testShow t
view g@(LevelPlayingState k t h) = testShow t

testShow :: Float -> IO Picture
testShow t = do
                animation <- harioIdleAnimation Small t harioSpeed
                return (scale 3 3 animation)