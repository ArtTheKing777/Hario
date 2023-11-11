{-# LANGUAGE BlockArguments #-}
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
view g@(StartScreenState k t mp _ _ _)  = loadUI g
view g@(LevelSelectState k t mp _ _ _)  = loadUI g
view g@(LevelPlayingState k t l@(Level h@(Hario pos _ _ _ _ _ _ _) _ _ _) a s) = do
                                    let level = l
                                    showLevel level t a
                                    
                                    


showLevel :: Level -> Float -> Map String BitmapData -> IO Picture
showLevel l@(Level h@(Hario pio@(px,py) Die _ _ _ _ _ _) e wio _) eT a = 
    return $ pictures[translate (-150) 0 $ scale 0.3 0.3 $ color black $ text "Game Over"
                     ,translate (-130) (-50) $ scale 0.1 0.1 $ color black $ text "Press space to restart"]
showLevel l@(Level h@(Hario pio@(px,py) Victory _ _ _ _ _ _) e wio c) eT a = 
    return $ pictures[translate (-150) 0 $ scale 0.3 0.3 $ color black $ text ("Completed Level:1-"++show c)
                     ,translate (-130) (-50) $ scale 0.1 0.1 $ color black $ text "Press space to go to level select"]
showLevel l@(Level h@(Hario pio@(px,py) _ _ _ _ _ cl coins) e wio c) eT a = do
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
        w = wio
        tilefP = checkgrid 0 w [a!"tilesBmp1",a!"coinsBmp",a!"tilesBmp2",a!"flagBmp",a!"pipeBmp"]
        harioanimation = animateHario h eT [a!"smallHarioAnimationSheetBmp",a!"harioAnimationSheetBmp",a!"fireHarioAnimationSheetBmp"]
        showLives = translate 250 200 $ scale 0.2 0.2 $ showInfo "Lives: " cl
        showLevel = translate 50 200 $ scale 0.2 0.2 $ showInfo "Level:1-" c 
        showTime = translate (-150) 200 $ scale 0.2 0.2 $ showInfo "Time: " (500-floor eT)
        showScore = translate (-350) 200 $ scale 0.2 0.2 $ showInfo "Coins: " coins
        showEnemies = Prelude.map (animateHenemy eT [a!"henemiesBmp",a!"howserBmp",a!"hammerBmp",a!"fireballBmp"
         ,a!"acidBmpSplat",a!"acidBmpMove",a!"wormBmpSpit",a!"wormBmpCharge",a!"wormBmpMove"]) e
        fP = tilefP ++ [harioanimation] ++ showEnemies
    return $ pictures[cameraTranspose pio (1,1) w (pictures fP),showLives,showLevel,showTime,showScore]

showInfo:: String -> Int -> Picture
showInfo s i = color white $ text(s ++ show i)

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
                Q i -> ((!!i). makeListofSheet2 (Rectangle (0,0) (16,-16)) 32) (getBmpIO fii)
                X i -> ((!!i). makeListofSheet2 (Rectangle (0,0) (16,-16)) 64) (getBmpIO fii)
                I i -> ((!!i). makeListofSheet2 (Rectangle (0,0) (16,-16)) 192) (getBmpIO fii)
                C   -> (animationLoop ti 0.2 . makeListofSheet2 (Rectangle (0,-1) (16,-16)) 48) (getBmpIO fii)
                fii -> bitmap (getBmpIO f)
        in getPicture f t

cameraTranspose:: Point -> Point -> [[Field]] -> Picture -> Picture
cameraTranspose (x,y) (zx,zy) g p = translate cx cy (scale zx zy p)
    where camBorderLeftX = x-400
          camBorderRightX = x+400
          camBorderDownY = y-225
          rightBorder = 16*fromIntegral(length (head g))
          downBorder = -16*fromIntegral(length g)
          cx | camBorderLeftX <= 0 = -400 
             | camBorderRightX >= rightBorder = -rightBorder+400
             | otherwise = -x
          cy | camBorderDownY <= downBorder = -downBorder-225
             | otherwise = -y

loadUI:: GameState -> IO Picture
loadUI (StartScreenState _ _ _ ui _ _) = return $ pictures $ Prelude.map getUIElemtpic ui
loadUI (LevelSelectState _ _ _ ui _ _) = return $ pictures $ Prelude.map getUIElemtpic ui

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
