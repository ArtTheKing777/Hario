{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Animations where
import Fileload
import Graphics.Gloss.Data.Bitmap (Rectangle (Rectangle), BitmapData (bitmapSize))
import Graphics.Gloss.Data.Picture (Picture (BitmapSection), scale, bitmap, translate)
import Data.Fixed (mod')
import GHC.Float (int2Float)
import Model
    ( EnemyType(..),
      Hario(Hario, direction, state, power, velocity),
      PlayerPower(..),
      Looking(Left),
      PlayerState(Swim, Idle, Walk, Jump, Fall, Die) )
import Prelude hiding (Left)

-- time, time per frame, pictures
animationLoop :: Float -> Float -> [Picture] -> Picture
animationLoop eT tF p = p !! floor (mod' (eT/tF) (int2Float (length p)))

--load frames as [Pictures]
getHarioFrames :: BitmapData -> [Picture]
getHarioFrames harioSheetBmp = makeListofSheet (Rectangle (0, -1) (17, -34)) harioSheetBmp (fst (bitmapSize harioSheetBmp))

getFireHarioFrames :: BitmapData -> [Picture]
getFireHarioFrames fireHarioSheetBmp = makeListofSheet (Rectangle (0, -36) (17, -35)) fireHarioSheetBmp (fst (bitmapSize fireHarioSheetBmp))

getSmallHarioFrames :: BitmapData -> [Picture]
getSmallHarioFrames smallHarioSheetBmp = makeListofSheet (Rectangle (0, -72) (17, -17)) smallHarioSheetBmp (fst (bitmapSize smallHarioSheetBmp))

getHenemyFrames :: BitmapData -> [Picture]
getHenemyFrames henemySheetBmp = makeListofSheet (Rectangle (0, -1) (17, -35)) henemySheetBmp (fst (bitmapSize henemySheetBmp))

getHammerFrames :: IO [Picture]
getHammerFrames = do
                    hammersheetbmp <- getHammerBmp
                    return (makeListofSheet (Rectangle (0, -1) (22, -17)) hammersheetbmp (fst (bitmapSize hammersheetbmp)))

getHireBallFrames :: IO [Picture]
getHireBallFrames = do
                        hireballsheetbmp <- getFireBmp
                        return (makeListofSheet (Rectangle (0, -1) (44, -17)) hireballsheetbmp (fst (bitmapSize hireballsheetbmp)))

getAcidFrames :: IO [Picture]
getAcidFrames = do
                    acidframesbmps <- getAcidBmp
                    let acidframes = makeListofSheet (Rectangle (0,-1) (80, -80)) (acidframesbmps !! 0) (fst (bitmapSize (acidframesbmps !! 0))) ++
                                        makeListofSheet (Rectangle (0, -1) (80, -80)) (acidframesbmps !! 1) (fst (bitmapSize (acidframesbmps !! 1)))
                    let scaledacidframes = map (scale 0.22 0.22) acidframes
                    return scaledacidframes
getWormFrames :: IO [Picture]
getWormFrames = do
                    wormframesbmps <- getWormBmp
                    let wormframes = makeListofSheet (Rectangle (0, -1) (80, -80)) (wormframesbmps !! 0) (fst (bitmapSize (wormframesbmps !! 0))) ++
                                        makeListofSheet (Rectangle (0, -1) (80, -80)) (wormframesbmps !! 1) (fst (bitmapSize (wormframesbmps !! 1))) ++
                                        [bitmap (wormframesbmps !! 2)]
                    let scaledwormframes = map (scale 0.22 0.22) wormframes
                    return scaledwormframes

getHowserFrames :: IO [Picture]
getHowserFrames = do
                    howserSheetBmp <- getHowserBmp
                    return (makeListofSheet (Rectangle (0, -1) (35, -35)) howserSheetBmp (fst (bitmapSize howserSheetBmp)))

getEnemyFrames :: EnemyType -> BitmapData -> IO [Picture]
getEnemyFrames e henemyBmp= do
                    let henemyframes = getHenemyFrames henemyBmp
                    case e of
                        Hoomba -> return [henemyframes !! 0, henemyframes !! 1, henemyframes !! 2]
                        HoopaTroopa -> return [henemyframes !! 3, henemyframes !! 4]
                        HoopaParaTroopa -> return [henemyframes !! 5, henemyframes !! 6]
                        HoopaShell -> return [henemyframes !! 7, henemyframes !! 8]
                        Hirrana -> return [henemyframes !! 9, henemyframes !! 10]
                        RedHirrana -> return [henemyframes !! 11, henemyframes !! 12]
                        HeepHeep -> return [henemyframes !! 13, henemyframes !! 14]
                        Hloober -> return [henemyframes !! 15, henemyframes !! 16]
                        Hakitu -> return [henemyframes !! 17, henemyframes !! 18]
                        HakituProjectile -> return [henemyframes !! 19, henemyframes !! 20]
                        HuzzyBeetle -> return [henemyframes !! 23, henemyframes !! 24]
                        Hiny -> return [henemyframes !! 21, henemyframes !! 22]
                        HoolitBill -> return [henemyframes !! 25]
                        HammerBrother -> return [henemyframes !! 26, henemyframes !! 27, henemyframes !! 28, henemyframes !! 29]
                        Howser -> getHowserFrames
                        Hammer -> getHammerFrames
                        HireBall -> getHireBallFrames
                        Hacid -> getAcidFrames
                        Worm -> getWormFrames

-- player animation sheets
idleSheet :: PlayerPower -> [BitmapData] -> [Picture]
idleSheet Small [a,b,c] = [head(getSmallHarioFrames a)]
idleSheet Big [a,b,c] = [head (getHarioFrames b)]
idleSheet Fire [a,b,c] = [head (getFireHarioFrames c)]

jumpSheet :: PlayerPower -> [BitmapData] -> [Picture]
jumpSheet Small [a,b,c] = [harioFrames !! 1, harioFrames !! 2]
                    where harioFrames = getSmallHarioFrames a
                    
jumpSheet Big [a,b,c] = [harioFrames !! 1, harioFrames !! 2]
                    where harioFrames = getHarioFrames b

jumpSheet Fire [a,b,c] = [harioFrames !! 1, harioFrames !! 2]
                    where harioFrames = getFireHarioFrames c

squatSheet :: PlayerPower -> [BitmapData] -> [Picture]
squatSheet Small [a,b,c] = [harioFrames !! 3]
                    where harioFrames = getSmallHarioFrames a
squatSheet Big [a,b,c] = [harioFrames !! 3]
                    where harioFrames = getHarioFrames b
squatSheet Fire [a,b,c] = [harioFrames !! 3]
                    where harioFrames = getFireHarioFrames c

walkSheet :: PlayerPower -> [BitmapData] -> [Picture]
walkSheet Small [a,b,c] = [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]
            where harioFrames = getSmallHarioFrames a
walkSheet Big [a,b,c] = [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]
            where harioFrames = getHarioFrames b
walkSheet Fire [a,b,c] = [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]
            where harioFrames = getFireHarioFrames c

swimSheet :: PlayerPower -> [BitmapData] -> [Picture]
swimSheet Small [a,b,c] = [harioFrames !! 8, harioFrames !! 9, harioFrames !! 10, harioFrames !! 12, harioFrames !! 13, harioFrames !! 14, harioFrames !! 7]
                    where harioFrames = getSmallHarioFrames a
swimSheet Big [a,b,c] = [harioFrames !! 8, harioFrames !! 9, harioFrames !! 10, harioFrames !! 12, harioFrames !! 13, harioFrames !! 14, harioFrames !! 7]
                    where harioFrames = getHarioFrames b
swimSheet Fire [a,b,c] = [harioFrames !! 8, harioFrames !! 9, harioFrames !! 10, harioFrames !! 12, harioFrames !! 13, harioFrames !! 14, harioFrames !! 7]
                    where harioFrames = getFireHarioFrames c

fallSheet :: PlayerPower -> [BitmapData] -> [Picture]
fallSheet Small [a,b,c] = [harioFrames !! 2]
                    where harioFrames = getSmallHarioFrames a
fallSheet Big [a,b,c] = [harioFrames !! 2]
                    where harioFrames = getHarioFrames b
fallSheet Fire [a,b,c] = [harioFrames !! 2]
                    where harioFrames = getFireHarioFrames c

-- fire hario only
fireShootSheet :: [BitmapData] -> [Picture]
fireShootSheet [a,b,c] = [harioFrames !! 15]
                    where harioFrames = getFireHarioFrames c

fireShootMoveSheet :: [BitmapData] -> [Picture]
fireShootMoveSheet [a,b,c]= [harioFrames !! 1, harioFrames !! 15, harioFrames !! 18, harioFrames !! 19, harioFrames !! 16, harioFrames !! 17, harioFrames !! 18]
                where harioFrames = getFireHarioFrames c

-- enemies



-- actual animations to be called.
-- hario power, eT, speed, to animations
harioIdleAnimation :: PlayerPower -> Float -> Float -> [BitmapData] -> Picture
harioIdleAnimation p eT s b = do
                                let harioSheet = idleSheet p b
                                if p == Small
                                    then animationLoop eT (1/s) harioSheet
                                    else animationLoop eT (0.5/s) harioSheet

harioJumpAnimation :: PlayerPower -> Float -> Float -> [BitmapData] -> Picture
harioJumpAnimation p eT s b = do
                                let harioSheet = jumpSheet p b
                                if p == Small
                                    then animationLoop eT (1/s) harioSheet
                                    else animationLoop eT (0.5/s) harioSheet

harioSquatAnimation :: PlayerPower -> Float -> Float -> [BitmapData] -> Picture
harioSquatAnimation p eT s b = do
                                let harioSheet = squatSheet p b
                                if p == Small
                                    then animationLoop eT (1/s) harioSheet
                                    else animationLoop eT (0.5/s) harioSheet

harioWalkAnimation :: PlayerPower -> Float -> Float -> [BitmapData] -> Picture
harioWalkAnimation p eT s b = do
                                let harioSheet = walkSheet p b
                                if p == Small
                                    then animationLoop eT (1/s) harioSheet
                                    else animationLoop eT (0.5/s) harioSheet

harioSwimAnimation :: PlayerPower -> Float -> Float -> [BitmapData] -> Picture
harioSwimAnimation p eT s b = do
                                let harioSheet = swimSheet p b
                                if p == Small
                                    then animationLoop eT (1/s) harioSheet
                                    else animationLoop eT (0.5/s) harioSheet

harioFallAnimation :: PlayerPower -> Float -> Float -> [BitmapData] -> Picture
harioFallAnimation p eT s b = do
                                let harioSheet = fallSheet p b
                                if p == Small
                                    then animationLoop eT (1/s) harioSheet
                                    else animationLoop eT (0.5/s) harioSheet


animateHario :: Hario -> Float -> [BitmapData] -> Picture
animateHario p@(Hario (x,y) _ _ _ _ _ _ _) t b = case state p of
                        Idle -> do
                                    let animation = harioIdleAnimation (power p) t (fst (velocity p)*10) b
                                    if direction p == Left then translate x y (scale (-1) 1 animation)
                                    else translate x y animation
                        Walk -> do
                                    let animation = harioWalkAnimation (power p) t (fst (velocity p)*10) b
                                    if direction p == Left then translate x y (scale (-1) 1 animation)
                                    else translate x y animation
                        Jump -> do
                                    let animation = harioJumpAnimation (power p) t (fst (velocity p)*10) b
                                    if direction p == Left then translate x y (scale (-1) 1 animation)
                                    else translate x y animation
                        Fall -> do
                                    let animation = harioFallAnimation (power p) t (fst (velocity p)*10) b
                                    if direction p == Left then translate x y (scale (-1) 1 animation)
                                    else translate x y animation
                        Die -> do
                                    let animation = harioSquatAnimation (power p) t (fst (velocity p)*10) b
                                    if direction p == Left then translate x y (scale (-1) 1 animation)
                                    else translate x y animation
                        Swim -> do
                                    let animation = harioSwimAnimation (power p) t (fst (velocity p)*10) b
                                    if direction p == Left then translate x y (scale (-1) 1 animation)
                                    else translate x y animation
-- | 270x-35px for normal hario
-- | 360x-71px from (0, -35) for fire hario
-- | 251x-89px from (0, -71) for small hario

-- | normal hario = 17x35px per sprite, fire hario is 1px taller
-- | small hario = 15x18px per sprite 