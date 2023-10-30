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

fps :: Int
fps = 60

harioSpeed :: Float
harioSpeed = 10

view :: GameState -> IO Picture
view g@(StartScreenState k t mp _)  = loadUI g
view g@(LevelSelectState k t mp _)  = loadUI g
view g@(LevelPlayingState k t l) = do
                                    level <- l
                                    loadLevel level t


loadLevel::Level -> Float -> IO Picture
loadLevel l@(Level h@(Hario pio _ _ _ _ _) e wio) eT = do
    let check c = case c of
            W i -> Just(W i)
            C   -> Just C
            Q i -> Just(Q i)
            X i -> Just(X i)
            I i -> Just(I i)
            c -> Nothing
        checkline xp yp l = case l of
                [] -> []
                (x:xs) -> let t = check x in
                    case t of
                        Just t -> fmap (translate (8+(16*xp)) (-8+(-16*yp))) (loadField t eT) : checkline (xp+1) yp xs
                        Nothing -> checkline (xp+1) yp xs
        checkgrid yp g = case g of
                [] -> []
                (y:ys) -> checkline 0 yp y ++ checkgrid (yp+1) ys
    w <- pure wio
    fP <- sequenceA (checkgrid 0 w)
    return (translate (-400) 0 (pictures fP))

loadField:: Field -> Float -> IO Picture
loadField f t = do
        let getBmpIO fi = case fi of
                W i -> fmap head getTilesBmp
                C   -> getCoinsBmp
                Q i -> fmap (!!1) getTilesBmp
                X i -> getFlagBmp
                I i -> getPipeBmp
            getPicture fii ti = case fii of
                W i -> fmap ((!!i). makeListofSheet2 (Rectangle (0,-1) (16,-16))) (getBmpIO fii)
                Q i -> fmap ((!!i). makeListofSheet2 (Rectangle (0,-1) (16,-16))) (getBmpIO fii)
                X i -> fmap ((!!i). makeListofSheet2 (Rectangle (0,-1) (16,-16))) (getBmpIO fii)
                I i -> fmap ((!!i). makeListofSheet2 (Rectangle (0,-1) (16,-16))) (getBmpIO fii)
                C   -> fmap (animationLoop ti 0.2 . makeListofSheet2 (Rectangle (0,-1) (16,-16))) (getBmpIO fii)
                fii -> fmap bitmap (getBmpIO f)
        getPicture f t

cameraTranspose:: (Float,Float) -> Picture -> Picture
cameraTranspose (x,y) p = translate (-16*x) (-16*y) p



loadUI:: GameState -> IO Picture
loadUI (StartScreenState _ _ _ ui) = pictures . map getUIElemtpic <$> ui

testShow :: Float -> Level -> IO Picture
testShow t l = do
                animation <- animateHario (player l) t
                return (scale 3 3 animation)

testMP :: IO (Float,Float) -> IO Picture
testMP mpio = do
    mp <- mpio
    return (scale 0.1 0.1 (color blue (text (show (fst mp) ++ " " ++ show (snd mp)))))

testButtonInput :: S.Set Key -> Picture
testButtonInput k = color blue (if (S.member (MouseButton LeftButton) k) then Text ("pressed") else Text ("no"))
