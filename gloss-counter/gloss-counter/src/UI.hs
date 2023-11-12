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

data UIElement = Button String (Float,Float) (Float,Float) Point Picture | LevelButton String (Float,Float) (Float,Float) Point Picture Picture Bool  | SomethingElse Picture

getUIElemtpic::UIElement -> Picture
getUIElemtpic (Button _ _ _ _ p) = p
getUIElemtpic (LevelButton _ _ _ _ p1 p2 b) = if b then p1 else p2
getUIElemtpic (SomethingElse p) = p

--button buttontext -> scale -> place -> picture
button:: String -> (Float,Float) -> Point -> Color -> BitmapData -> UIElement
button buttontext s@(sx,sy) p@(x,y)  c textboxp = do
                                    let textbox = translate x y (scale sx sy (bitmap textboxp))
                                        textinbox = translate (x-(150*sx)) (y-(57.5*sy)) (scale sx sy (color c(Text buttontext )))
                                        width =  int2Float (fst (bitmapSize textboxp)) * sx
                                        heigth = int2Float (snd (bitmapSize textboxp)) * sy
                                    Button buttontext s (width,heigth) p (pictures [textbox,textinbox])

levelbutton:: String -> (Float,Float) -> Point -> Color -> [BitmapData] -> Bool -> UIElement
levelbutton buttontext s@(sx,sy) p@(x,y) c bt b = do
                                    let textboxp = head bt
                                        textboxc = bt!!1
                                        textbox = translate x y (scale sx sy (bitmap textboxp))
                                        textbox2 = translate x y (scale sx sy (bitmap textboxc))
                                        textinbox = translate (x-(150*sx)) (y-(57.5*sy)) (scale sx sy (color c(Text buttontext )))
                                        width =  int2Float (fst (bitmapSize textboxp)) * sx
                                        heigth = int2Float (snd (bitmapSize textboxp)) * sy
                                    LevelButton buttontext s (width,heigth) p (pictures [textbox,textinbox]) textbox2 b

somethingElse:: BitmapData -> (Float,Float) -> Point -> UIElement
somethingElse fp (sx,sy) (x,y) = do
                                    let textboxp = fp
                                        textbox = translate x y (scale sx sy (bitmap textboxp))
                                    SomethingElse textbox

hoveredButton::(Float,Float) -> UIElement -> Bool
hoveredButton (mx,my) (Button t s (width,heigth) (bx,by) _) | (mx > toplx) && (mx < botlx) && (my < toply) && (my > botly)  = True
                                                            | otherwise = False
    where
          toplx = bx - (width / 2)
          toply = by + (heigth / 2)
          botlx = bx + (width / 2)
          botly = by - (heigth / 2)
hoveredButton (mx,my) (LevelButton t s (width,heigth) (bx,by) _ _ _ ) | (mx > toplx) && (mx < botlx) && (my < toply) && (my > botly)  = True
                                                            | otherwise = False
    where
          toplx = bx - (width / 2)
          toply = by + (heigth / 2)
          botlx = bx + (width / 2)
          botly = by - (heigth / 2)
hoveredButton (mx,my) u = False



