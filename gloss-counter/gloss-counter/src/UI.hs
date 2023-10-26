module UI where
import Controller
import Fileload
import qualified Data.Set as S

import Graphics.Gloss.Data.Picture (Picture (Bitmap, BitmapSection, Blank) , pictures, scale, translate, bitmapSection, bitmap)
import Model
import Animations
import Graphics.Gloss.Interface.IO.Animate (animateIO)
import Data.Fixed
import GHC.Float (int2Float)
import Graphics.Gloss
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game ( Point, Key (MouseButton), MouseButton (LeftButton) )

--button buttontext -> scale -> place -> picture
button:: String -> (Float,Float) -> Point -> IO Picture
button buttontext (sx,sy) (x,y) = do
                                    textboxp <- getTextBox
                                    let textbox = translate x y (scale sx sy (bitmap textboxp))
                                        textinbox = translate (x-(150*sx)) (y-(57.5*sy)) (scale sx sy (color black(Text buttontext )))
                                    return (pictures [textbox,textinbox])




