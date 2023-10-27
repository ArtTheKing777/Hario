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

data UIElement = Button String (Float,Float) (Float,Float) Point Picture | SomethingElse Picture

getUIElemtpic::UIElement -> Picture
getUIElemtpic (Button _ _ _ _ p) = p
getUIElemtpic (SomethingElse p) = p

--button buttontext -> scale -> place -> picture
button:: String -> (Float,Float) -> Point -> Color -> IO UIElement
button buttontext s@(sx,sy) p@(x,y)  c = do
                                    textboxp <- getTextBoxBmp
                                    let textbox = translate x y (scale sx sy (bitmap textboxp))
                                        textinbox = translate (x-(150*sx)) (y-(57.5*sy)) (scale sx sy (color c(Text buttontext )))
                                        width =  int2Float (fst (bitmapSize textboxp)) * sx
                                        heigth = int2Float (snd (bitmapSize textboxp)) * sy
                                    return (Button buttontext s (width,heigth) p (pictures [textbox,textinbox]))

somethingElse:: IO BitmapData -> (Float,Float) -> Point -> IO UIElement
somethingElse fp (sx,sy) (x,y) = do
                                    textboxp <- fp
                                    let textbox = translate x y (scale sx sy (bitmap textboxp))
                                    return (SomethingElse textbox)

hoveredButton::(Float,Float) -> UIElement -> Bool
hoveredButton (mx,my) (Button t s (width,heigth) (bx,by) _) | (mx > toplx) && (mx < botlx) && (my < toply) && (my > botly)  = True
                                                            | otherwise = False
    where
          toplx = bx - (width / 2)
          toply = by + (heigth / 2)
          botlx = bx + (width / 2)
          botly = by - (heigth / 2)
hoveredButton (mx,my) u = False




