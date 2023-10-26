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
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game ( Point, Key (MouseButton), MouseButton (LeftButton) )

fps :: Int
fps = 60

harioSpeed :: Float
harioSpeed = 10

view :: GameState -> IO Picture
view g@(StartScreenState k t mp)  = return (testMP mp)
view g@(LevelSelectState k t)  = return blank
view g@(LevelPlayingState k t l) = testShow t l

testShow :: Float -> Level -> IO Picture
testShow t l = do
                frames <- getEnemyFrames Worm
                let animation = animationLoop t 0.5 frames
                return (scale 3 3 animation)

testMP :: (Float,Float) -> Picture
testMP mp = scale 0.1 0.1 (color blue (text (show (fst mp) ++ " " ++ show (snd mp))))

testButtonInput :: S.Set Key -> Picture
testButtonInput k = color blue (if (S.member (MouseButton LeftButton) k) then Text ("pressed") else Text ("no"))
