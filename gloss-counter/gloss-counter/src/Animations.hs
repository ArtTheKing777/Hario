{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use head" #-}
module Animations where
import Fileload
import Graphics.Gloss.Data.Bitmap (Rectangle (Rectangle), BitmapData (bitmapSize))
import Graphics.Gloss.Data.Picture (Picture (BitmapSection, Blank), scale, bitmap, translate)
import Data.Fixed (mod')
import GHC.Float (int2Float)
import Model
import Prelude hiding (Left)
import Model (Enemy)

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

getHammerFrames :: BitmapData -> [Picture]
getHammerFrames hammersheetbmp = makeListofSheet (Rectangle (0, -1) (22, -17)) hammersheetbmp (fst (bitmapSize hammersheetbmp))

getHireBallFrames :: BitmapData -> [Picture]
getHireBallFrames hireballsheetbmp = makeListofSheet (Rectangle (0, -1) (8, -8)) hireballsheetbmp (fst (bitmapSize hireballsheetbmp))

getAcidFrames :: [BitmapData] -> [Picture]
getAcidFrames acidframesbmps = scaledacidframes
                            where acidframes = makeListofSheet (Rectangle (0,-1) (80, -80)) (acidframesbmps !! 0) (fst (bitmapSize (acidframesbmps !! 0))) ++
                                        makeListofSheet (Rectangle (0, -1) (80, -80)) (acidframesbmps !! 1) (fst (bitmapSize (acidframesbmps !! 1)))
                                  scaledacidframes = map (scale 0.22 0.22) acidframes  
                    
getWormFrames :: [BitmapData] -> [Picture]
getWormFrames wormframesbmps = scaledwormframes
                            where wormframes = makeListofSheet (Rectangle (0, -1) (80, -80)) (wormframesbmps !! 0) (fst (bitmapSize (wormframesbmps !! 0))) ++
                                        makeListofSheet (Rectangle (0, -1) (80, -80)) (wormframesbmps !! 1) (fst (bitmapSize (wormframesbmps !! 1))) ++
                                        [bitmap (wormframesbmps !! 2)]
                                  scaledwormframes = map (scale 0.45 0.45) wormframes

getHowserFrames :: BitmapData -> [Picture]
getHowserFrames howserSheetBmp = makeListofSheet (Rectangle (0, -1) (35, -35)) howserSheetBmp (fst (bitmapSize howserSheetBmp))

getEnemyFrames :: EnemyType -> [BitmapData] -> EnemyState -> [Picture]
getEnemyFrames e henemyBmp s = case e of
                                    Hoomba -> case s of
                                                EIdle -> [henemyframes !! 0]
                                                EWalk -> [henemyframes !! 0, henemyframes !! 1]
                                                EAttack -> [henemyframes !! 0, henemyframes !! 1]
                                                EDie -> [henemyframes !! 2]
                                                EDead -> []   
                                    HoopaTroopa -> case s of
                                                        EIdle -> [henemyframes !! 3]
                                                        EWalk -> [henemyframes !! 3, henemyframes !! 4]
                                                        EAttack -> [henemyframes !! 3, henemyframes !! 4]
                                                        EDie -> [henemyframes !! 8]
                                                        EDead -> [henemyframes !! 7]
                                    HoopaParaTroopa -> case s of
                                                        EIdle -> [henemyframes !! 5]
                                                        EWalk -> [henemyframes !! 5, henemyframes !! 6]
                                                        EAttack -> [henemyframes !! 5, henemyframes !! 6]
                                                        EDie -> [henemyframes !! 8]
                                                        EDead -> [henemyframes !! 7]
                                    HoopaShell -> case s of
                                                        EIdle -> [henemyframes !! 7]
                                                        EWalk -> [henemyframes !! 7]
                                                        EAttack -> [henemyframes !! 7]
                                                        EDie -> [henemyframes !! 7]
                                                        EDead -> []
                                    Hirrana -> case s of
                                                        EIdle -> [henemyframes !! 9]
                                                        EWalk -> [henemyframes !! 9, henemyframes !! 10]
                                                        EAttack -> [henemyframes !! 9, henemyframes !! 10]
                                                        EDie -> [henemyframes !! 9]
                                                        EDead -> []
                                    RedHirrana -> case s of
                                                        EIdle -> [henemyframes !! 11]
                                                        EWalk -> [henemyframes !! 11, henemyframes !! 12]
                                                        EAttack -> [henemyframes !! 11, henemyframes !! 12]
                                                        EDie -> [henemyframes !! 11]
                                                        EDead -> []
                                    HeepHeep -> case s of
                                                        EIdle -> [henemyframes !! 13]
                                                        EWalk -> [henemyframes !! 13, henemyframes !! 14]
                                                        EAttack -> [henemyframes !! 13, henemyframes !! 14]
                                                        EDie -> [henemyframes !! 14]
                                                        EDead -> []
                                    Hloober -> case s of
                                                        EIdle -> [henemyframes !! 15]
                                                        EWalk -> [henemyframes !! 15, henemyframes !! 16]
                                                        EAttack -> [henemyframes !! 15, henemyframes !! 16]
                                                        EDie -> [henemyframes !! 16]
                                                        EDead -> []
                                    Hakitu -> case s of
                                                        EIdle -> [henemyframes !! 17]
                                                        EWalk -> [henemyframes !! 17]
                                                        EAttack -> [henemyframes !! 17]
                                                        EDie -> [henemyframes !! 18]
                                                        EDead -> []
                                    HakituProjectile -> case s of
                                                        EIdle -> [henemyframes !! 19, henemyframes !! 20]
                                                        EWalk -> [henemyframes !! 19, henemyframes !! 20]
                                                        EAttack -> [henemyframes !! 19, henemyframes !! 20]
                                                        EDie -> [henemyframes !! 19, henemyframes !! 20]
                                                        EDead -> []
                                    HuzzyBeetle -> case s of
                                                        EIdle -> [henemyframes !! 23]
                                                        EWalk -> [henemyframes !! 23, henemyframes !! 24]
                                                        EAttack -> [henemyframes !! 23, henemyframes !! 24]
                                                        EDie -> [henemyframes !! 25]
                                                        EDead -> []
                                    Hiny -> case s of
                                                        EIdle -> [henemyframes !! 21]
                                                        EWalk -> [henemyframes !! 21, henemyframes !! 22]
                                                        EAttack -> [henemyframes !! 21, henemyframes !! 22]
                                                        EDie -> [henemyframes !! 21]
                                                        EDead -> []
                                    HoolitBill -> case s of
                                                        EIdle -> [henemyframes !! 26]
                                                        EWalk -> [henemyframes !! 26]
                                                        EAttack -> [henemyframes !! 26]
                                                        EDie -> [henemyframes !! 26]
                                                        EDead -> []
                                    HammerBrother -> case s of
                                                        EIdle -> [henemyframes !! 29]
                                                        EWalk -> [henemyframes !! 29, henemyframes !! 30]
                                                        EAttack -> [henemyframes !! 27, henemyframes !! 28]
                                                        EDie -> [henemyframes !! 28]
                                                        EDead -> []
                                    Howser -> case s of
                                                        EIdle -> [howserFrames !! 0]
                                                        EWalk -> [howserFrames !! 2, howserFrames !! 3]
                                                        EAttack -> [howserFrames !! 0, howserFrames !! 1]
                                                        EDie -> [howserFrames !! 0]
                                                        EDead -> []
                                    Hammer -> case s of
                                                        EIdle -> hammerFrames
                                                        EWalk -> hammerFrames
                                                        EAttack -> hammerFrames
                                                        EDie -> hammerFrames
                                                        EDead -> []
                                    HireBall -> case s of
                                                        EIdle -> hireBallFrames
                                                        EWalk -> hireBallFrames
                                                        EAttack -> hireBallFrames
                                                        EDie -> hireBallFrames
                                                        EDead -> []
                                    Hacid -> case s of
                                                        EIdle -> []
                                                        EWalk -> [acidFrames !! 0, acidFrames !! 1, acidFrames !! 2, acidFrames !! 3, acidFrames !! 4, acidFrames !! 5, acidFrames !! 6]
                                                        EAttack -> []
                                                        EDie -> [acidFrames !! 7, acidFrames !! 8, acidFrames !! 9, acidFrames !!  10, acidFrames !! 11, acidFrames !! 12]
                                                        EDead -> []
                                    Worm -> case s of
                                                        EIdle -> [wormFrames !! 4]
                                                        EWalk -> [wormFrames !! 0, wormFrames !! 1, wormFrames !! 2, wormFrames !! 3]
                                                        EAttack -> [wormFrames !! 5, wormFrames !! 6, wormFrames !! 7, wormFrames !! 8, wormFrames !! 9, wormFrames !! 10, wormFrames !! 11]
                                                        EDie -> [wormFrames !! 4]
                                                        EDead -> []
                                where   henemyframes = getHenemyFrames (henemyBmp !! 0) 
                                        howserFrames = getHowserFrames (henemyBmp !! 1)
                                        hammerFrames = getHammerFrames (henemyBmp !! 2)
                                        hireBallFrames = getHireBallFrames (henemyBmp !! 3)
                                        acidFrames = getAcidFrames [henemyBmp !! 4, henemyBmp !! 5]
                                        wormFrames = getWormFrames [henemyBmp !! 6, henemyBmp !! 7, henemyBmp !! 8]

animateHenemy :: Float -> [BitmapData] -> Enemy ->  Picture
animateHenemy eT bmps e@(Enemy (x,y) t s l) = do
                                                    let hframes = getEnemyFrames t bmps s
                                                    let hanimation = animationLoop eT (1/2) hframes
                                                    if l == Left then translate x y (scale (-1) 1 hanimation)
                                                    else translate x y hanimation

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
animateHario p@(Hario (x,y) _ _ _ _ _) t b = case state p of
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