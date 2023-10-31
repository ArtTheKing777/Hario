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
import UI (UIElement(Button), getUIElemtpic)
import Fileload
import Data.Map

fps :: Int
fps = 60

harioSpeed :: Float
harioSpeed = 10

view :: GameState -> IO Picture
view g@(StartScreenState k t mp _ _)  = loadUI g
view g@(LevelSelectState k t mp _ _)  = loadUI g
view g@(LevelPlayingState k t l a) = do
                                    level <- l
                                    pic <- showLevel level t a
                                    return (pictures [pic, testTime t])


showLevel :: Level -> Float -> Map String BitmapData -> IO Picture
showLevel l@(Level h@(Hario pio _ _ _ _ _) e wio) eT a = do
    let check c = case c of
            W i -> Just (W i)
            C   -> Just C
            Q i -> Just (Q i)
            X i -> Just (X i)
            I i -> Just (I i)
            c -> Nothing
        checkline xp yp l i = case l of
                [] -> []
                (x:xs) -> let t = check x in
                    case t of
                        Just t -> translate (8+(16*xp)) (-8+(-16*yp)) (showField t eT i) : checkline (xp+1) yp xs i
                        Nothing -> checkline (xp+1) yp xs i
        checkgrid yp g i = case g of
                [] -> []
                (y:ys) -> checkline 0 yp y i ++ checkgrid (yp+1) ys i
    w <- pure wio
    let tilefP = checkgrid 0 w [a!"smallHarioAnimationSheetBmp",a!"harioAnimationSheetBmp",a!"fireHarioAnimationSheetBmp",a!"harioAnimationSheetBmp",a!"harioAnimationSheetBmp"]
    let harioanimation = animateHario h eT [a!"smallHarioAnimationSheetBmp",a!"harioAnimationSheetBmp",a!"fireHarioAnimationSheetBmp"]
    let fP = tilefP ++ [harioanimation]
    return (translate (-400) 0 (pictures fP))

showField:: Field -> Float -> [BitmapData] -> Picture
showField f t [a,b,c,d,e] =
        let getBmpIO fi = case fi of
                W i -> a
                C   -> b
                Q i -> c
                X i -> d
                I i -> e
            getPicture fii ti = case fii of
                W i -> ((!!i). makeListofSheet2 (Rectangle (0,0) (16,-16)) 190) (getBmpIO fii)
                Q i -> ((!!i). makeListofSheet2 (Rectangle (0,0) (16,-16)) 16) (getBmpIO fii)
                X i -> ((!!i). makeListofSheet2 (Rectangle (0,0) (16,-16)) 48) (getBmpIO fii)
                I i -> ((!!i). makeListofSheet2 (Rectangle (0,0) (16,-16)) 192) (getBmpIO fii)
                C   -> (animationLoop ti 0.2 . makeListofSheet2 (Rectangle (0,-1) (16,-16)) 48) (getBmpIO fii)
                fii -> bitmap (getBmpIO f)
        in getPicture f t

cameraTranspose:: Point -> Picture -> Picture
cameraTranspose (x,y) = translate (-16*x) (-16*y)



loadUI:: GameState -> IO Picture
loadUI (StartScreenState _ _ _ ui _) = pictures . Prelude.map getUIElemtpic <$> ui

testShow :: Float -> Level -> Map String BitmapData -> IO Picture
testShow t l a = do
                let animation = animateHario (player l) t [a!"smallHarioAnimationSheetBmp",a!"harioAnimationSheetBmp",a!"fireHarioAnimationSheetBmp"]
                return (scale 3 3 animation)

testMP :: IO (Float,Float) -> IO Picture
testMP mpio = do
    mp <- mpio
    return (scale 0.1 0.1 (color blue (text (show (fst mp) ++ " " ++ show (snd mp)))))

testButtonInput :: S.Set Key -> Picture
testButtonInput k = color blue (if (S.member (MouseButton LeftButton) k) then Text ("pressed") else Text ("no"))

testTime :: Float -> Picture
testTime eT = text (show eT)
