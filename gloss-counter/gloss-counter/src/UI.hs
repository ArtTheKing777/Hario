module UI where
import Fileload
import qualified Data.Set as S

import Graphics.Gloss.Data.Picture (Picture (Bitmap, BitmapSection, Blank) , pictures, scale, translate, bitmapSection, bitmap)
import Graphics.Gloss.Interface.IO.Animate (animateIO)
import Data.Fixed
import GHC.Float (int2Float)
import Graphics.Gloss
import qualified Data.Set as S
import Graphics.Gloss.Interface.IO.Game ( Point, Key (MouseButton), MouseButton (LeftButton) )

data UIElement = Button Picture | SomethingElse Picture

getUIElemtpic::UIElement -> Picture
getUIElemtpic (Button p) = p
getUIElemtpic (SomethingElse p) = p

--button buttontext -> scale -> place -> picture
button:: String -> (Float,Float) -> Point -> IO Picture
button buttontext (sx,sy) (x,y) = do
                                    textboxp <- getTextBoxBmp
                                    let textbox = translate x y (scale sx sy (bitmap textboxp))
                                        textinbox = translate (x-(150*sx)) (y-(57.5*sy)) (scale sx sy (color black(Text buttontext )))
                                    return (pictures [textbox,textinbox])




