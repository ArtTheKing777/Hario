{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Animations where
import Fileload
import Graphics.Gloss.Data.Bitmap (Rectangle (Rectangle), BitmapData (bitmapSize))
import Graphics.Gloss.Data.Picture (Picture (BitmapSection), scale, bitmap, translate)
import Data.Fixed (mod')
import GHC.Float (int2Float)
import Model
import Prelude hiding (Left)

-- time, time per frame, pictures
animationLoop :: Float -> Float -> [Picture] -> Picture
animationLoop eT tF p = p !! floor (mod' (eT/tF) (int2Float (length p)))

--load frames as [Pictures]
getHarioFrames :: IO [Picture]
getHarioFrames = do
                    harioSheetBmp <- getHarioAnimationSheetBmp
                    return (makeListofSheet (Rectangle (0, -1) (17, -35)) harioSheetBmp (fst (bitmapSize harioSheetBmp)))
getFireHarioFrames :: IO [Picture]
getFireHarioFrames = do
                        fireHarioSheetBmp <- getFireHarioAnimationSheetBmp
                        return (makeListofSheet (Rectangle (0, -36) (17, -36)) fireHarioSheetBmp (fst (bitmapSize fireHarioSheetBmp)))
getSmallHarioFrames :: IO [Picture]
getSmallHarioFrames = do
                        smallHarioSheetBmp <- getSmallHarioAnimationSheetBmp
                        return (makeListofSheet (Rectangle (0, -72) (17, -18)) smallHarioSheetBmp (fst (bitmapSize smallHarioSheetBmp)))

getHenemyFrames :: IO [Picture]
getHenemyFrames = do
                    henemySheetBmp <- getHenemiesBmp
                    return (makeListofSheet (Rectangle (0, -1) (17, -35)) henemySheetBmp (fst (bitmapSize henemySheetBmp)))

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

getEnemyFrames :: EnemyType -> IO [Picture]
getEnemyFrames e = do
                    henemyframes <- getHenemyFrames
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
idleSheet :: PlayerPower -> IO [Picture]
idleSheet Small = do
                    harioFrames <- getSmallHarioFrames
                    return [head harioFrames]
idleSheet Big = do
                    harioFrames <- getHarioFrames
                    return [head harioFrames]
idleSheet Fire = do
                    harioFrames <- getFireHarioFrames
                    return [head harioFrames]

jumpSheet :: PlayerPower -> IO [Picture]
jumpSheet Small = do
                    harioFrames <- getSmallHarioFrames
                    return [harioFrames !! 1, harioFrames !! 2]
jumpSheet Big = do
                    harioFrames <- getHarioFrames
                    return [harioFrames !! 1, harioFrames !! 2]
jumpSheet Fire = do
                    harioFrames <- getFireHarioFrames
                    return [harioFrames !! 1, harioFrames !! 2]

squatSheet :: PlayerPower -> IO [Picture]
squatSheet Small = do
                    harioFrames <- getSmallHarioFrames
                    return [harioFrames !! 3]
squatSheet Big = do
                    harioFrames <- getHarioFrames
                    return [harioFrames !! 3]
squatSheet Fire = do
                    harioFrames <- getFireHarioFrames
                    return [harioFrames !! 3]

walkSheet :: PlayerPower -> IO [Picture]
walkSheet Small = do
                        harioFrames <- getSmallHarioFrames
                        return [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]
walkSheet Big = do
                        harioFrames <- getHarioFrames
                        return [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]
walkSheet Fire = do
                        harioFrames <- getFireHarioFrames
                        return [harioFrames !! 4, harioFrames !! 5, harioFrames !! 6, harioFrames !! 5]

swimSheet :: PlayerPower -> IO [Picture]
swimSheet Small = do
                    harioFrames <- getSmallHarioFrames
                    return [harioFrames !! 8, harioFrames !! 9, harioFrames !! 10, harioFrames !! 12, harioFrames !! 13, harioFrames !! 14, harioFrames !! 7]
swimSheet Big = do
                    harioFrames <- getHarioFrames
                    return [harioFrames !! 8, harioFrames !! 9, harioFrames !! 10, harioFrames !! 12, harioFrames !! 13, harioFrames !! 14, harioFrames !! 7]
swimSheet Fire = do
                    harioFrames <- getHarioFrames
                    return [harioFrames !! 8, harioFrames !! 9, harioFrames !! 10, harioFrames !! 12, harioFrames !! 13, harioFrames !! 14, harioFrames !! 7]

fallSheet :: PlayerPower -> IO [Picture]
fallSheet Small = do
                    harioFrames <- getSmallHarioFrames
                    return [harioFrames !! 2]
fallSheet Big = do
                    harioFrames <- getHarioFrames
                    return [harioFrames !! 2]
fallSheet Fire = do
                    harioFrames <- getFireHarioFrames
                    return [harioFrames !! 2]

-- fire hario only
fireShootSheet :: IO [Picture]
fireShootSheet = do
                    harioFrames <- getFireHarioFrames
                    return [harioFrames !! 15]

fireShootMoveSheet :: IO [Picture]
fireShootMoveSheet = do
                    harioFrames <- getFireHarioFrames
                    return [harioFrames !! 1, harioFrames !! 15, harioFrames !! 18, harioFrames !! 19, harioFrames !! 16, harioFrames !! 17, harioFrames !! 18]

-- enemies



-- actual animations to be called.
-- hario power, eT, speed, to animations
harioIdleAnimation :: PlayerPower -> Float -> Float -> IO Picture
harioIdleAnimation p eT s = do
                                harioSheet <- idleSheet p
                                if p == Small
                                    then return (animationLoop eT (1/s) harioSheet)
                                    else return (animationLoop eT (0.5/s) harioSheet)

harioJumpAnimation :: PlayerPower -> Float -> Float -> IO Picture
harioJumpAnimation p eT s = do
                                harioSheet <- jumpSheet p
                                if p == Small
                                    then return (animationLoop eT (1/s) harioSheet)
                                    else return (animationLoop eT (0.5/s) harioSheet)

harioSquatAnimation :: PlayerPower -> Float -> Float -> IO Picture
harioSquatAnimation p eT s = do
                                harioSheet <- squatSheet p
                                if p == Small
                                    then return (animationLoop eT (1/s) harioSheet)
                                    else return (animationLoop eT (0.5/s) harioSheet)

harioWalkAnimation :: PlayerPower -> Float -> Float -> IO Picture
harioWalkAnimation p eT s = do
                                harioSheet <- walkSheet p
                                if p == Small
                                    then return (animationLoop eT (1/s) harioSheet)
                                    else return (animationLoop eT (0.5/s) harioSheet)

harioSwimAnimation :: PlayerPower -> Float -> Float -> IO Picture
harioSwimAnimation p eT s = do
                                harioSheet <- swimSheet p
                                if p == Small
                                    then return (animationLoop eT (1/s) harioSheet)
                                    else return (animationLoop eT (0.5/s) harioSheet)

harioFallAnimation :: PlayerPower -> Float -> Float -> IO Picture
harioFallAnimation p eT s = do
                                harioSheet <- fallSheet p
                                if p == Small
                                    then return (animationLoop eT (1/s) harioSheet)
                                    else return (animationLoop eT (0.5/s) harioSheet)


animateHario :: Hario -> Float -> IO Picture
animateHario p@(Hario (x,y) _ _ _ _ _) t = case state p of
                        Idle -> do
                                    animation <- harioIdleAnimation (power p) t (fst (velocity p)*10)
                                    if direction p == Left then return (translate x y (scale (-1) 1 animation))
                                    else return (translate x y animation)
                        Walk -> do
                                    animation <- harioWalkAnimation (power p) t (fst (velocity p)*10)
                                    if direction p == Left then return (translate x y (scale (-1) 1 animation))
                                    else return (translate x y animation)
                        Jump -> do
                                    animation <- harioJumpAnimation (power p) t (fst (velocity p)*10)
                                    if direction p == Left then return (translate x y (scale (-1) 1 animation))
                                    else return (translate x y animation)
                        Fall -> do
                                    animation <- harioFallAnimation (power p) t (fst (velocity p)*10)
                                    if direction p == Left then return (translate x y (scale (-1) 1 animation))
                                    else return (translate x y animation)
                        Die -> do
                                    animation <- harioSquatAnimation (power p) t (fst (velocity p)*10)
                                    if direction p == Left then return (translate x y (scale (-1) 1 animation))
                                    else return (translate x y animation)
                        Swim -> do
                                    animation <- harioSwimAnimation (power p) t (fst (velocity p)*10)
                                    if direction p == Left then return (translate x y (scale (-1) 1 animation))
                                    else return (translate x y animation)
-- | 270x-35px for normal hario
-- | 360x-71px from (0, -35) for fire hario
-- | 251x-89px from (0, -71) for small hario

-- | normal hario = 17x35px per sprite, fire hario is 1px taller
-- | small hario = 15x18px per sprite 