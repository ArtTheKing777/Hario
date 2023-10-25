module UI where
import Controller
import Fileload
import qualified Data.Set as S

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import Graphics.Gloss.Data.Picture (Picture (Bitmap, BitmapSection, Blank) , pictures, scale, translate, bitmapSection, bitmap)

--button buttontext -> scale -> place -> picture
button:: String -> (Float,Float) -> Point -> Picture
button = translate getTextBox

