

module Controller where

import Model
import qualified Data.Set as S

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import UI (UIElement (Button, SomethingElse, LevelButton), button, hoveredButton, levelbutton)
import GHC.Float (int2Float)
import Hario
import Data.List (elemIndex)
import Fileload (saveHario)
import Data.Map (Map, (!))

-- | Handle one iteration of the game | eT = elaspsedTme
update_ :: Float -> GameState -> IO GameState
update_ eT s@(LevelSelectState k t mp ui a l) = updateUI (t + eT) s a
update_ eT s@(StartScreenState k t mp ui a l) = updateUI (t + eT) s a
update_ eT s@(LevelPlayingState k t h a l) = updateLevelState eT s

updateUI :: Float -> GameState -> Map String BitmapData -> IO GameState
updateUI eT s@(LevelPlayingState {}) dic = return s
updateUI eT s dic = do
        let uic = ui s
            mp = mousePos s
            k = keys s
            news u = case u of
                [] -> []
                (x:xs) -> case x of
                        (Button t s _ p pic) -> if hoveredButton mp x
                                              then button t s p blue (dic!"textBoxBmp") : news xs
                                              else button t s p black (dic!"textBoxBmp") : news xs
                        (LevelButton t s _ p pic1 pic2 bool) -> if hoveredButton mp x && bool
                                              then levelbutton t s p blue [dic!"textBoxBmp",dic!"textBoxBmpNo"] bool : news xs
                                              else levelbutton t s p black [dic!"textBoxBmp",dic!"textBoxBmpNo"] bool : news xs
                        (SomethingElse pic)  -> x : news xs
            newstate = case whichButtonPressed uic mp of
                (Just (Button tx _ _ _ _)) -> (if S.member (MouseButton LeftButton) k
                    then buttonPressedActions tx s
                    else s{ui = news uic})
                (Just (LevelButton tx _ _ _ _ _ _)) -> (if S.member (MouseButton LeftButton) k
                    then buttonPressedActions tx s
                    else s{ui = news uic})
                Nothing -> s{ui = news uic}
        return newstate

whichButtonPressed:: [UIElement] -> (Float,Float) -> Maybe UIElement
whichButtonPressed [] mp = Nothing
whichButtonPressed (x@(Button t s b p pic) :xs) mp | hoveredButton mp x = Just x
                                                   | otherwise = whichButtonPressed xs mp
whichButtonPressed (x@(LevelButton t s b p pic1 pic2 bool) :xs) mp | hoveredButton mp x && bool = Just x
                                                                   | otherwise = whichButtonPressed xs mp
whichButtonPressed (x:xs) mp = whichButtonPressed xs mp



buttonPressedActions:: String -> GameState -> GameState
buttonPressedActions b s = case b of
    "start" -> initialLevelSelectState (loadedAnimations s) (loadedLevels s)
    "1" -> initialLevelPlayingState (loadedAnimations s) (loadedLevels s) 1
    "2" -> initialLevelPlayingState (loadedAnimations s) (loadedLevels s) 2
    b -> s

updateLevelState :: Float -> GameState -> IO GameState
updateLevelState eT g@(LevelPlayingState k t z@(Level (Hario (x,y) Die p o v m l co) e w c) a n)
    | l > 0 = return $ LevelPlayingState k 0 (reCreateLevel c n (Hario (x,y) Idle p o v m (l-1) co)) a n
    | otherwise = if S.member (SpecialKey KeySpace) k
                  then do
                    let ch = createEmptyHarioSave
                    save <- saveHario ch
                    return (initialLevelSelectState a (ch : tail n))
                  else return g
updateLevelState eT g@(LevelPlayingState k t z@(Level h@(Hario (x,y) Victory p o v m l co) e w c) a n) =
    if S.member (SpecialKey KeySpace) k
    then do
        let ch = createHarioSave h c (head n)
        save <- saveHario ch
        return (initialLevelSelectState a (ch : tail n))
    else return g
updateLevelState eT (LevelPlayingState k t l a s) = let
                                                        mover i = if S.member (Char 'd') k
                                                                then moveRight . i
                                                                else i
                                                        movel i = if S.member (Char 'a') k
                                                                then moveLeft . i
                                                                else i
                                                        jump_ i = if S.member (SpecialKey KeySpace) k
                                                               then jump . i
                                                               else i
                                                        keysupdate c = if S.member (SpecialKey KeySpace) c
                                                               then S.delete (SpecialKey KeySpace) c
                                                               else c
                                                        combine = update' (eT+t) l (mover $ movel $ jump_ $ idle)
                                                        in return (LevelPlayingState (keysupdate k) (eT+t) combine a s)

update' :: Float -> Level -> (Hario -> Hario) -> Level
update' eT l@(Level p e g c) m = worldGridUpdate $ Level (
    deathByTime eT
    $ deathByFalling g
    $ updateHario
    $ enemyCollideCheck e
    $ checkFor1Up
    $ tileCollisionCheck g
    $ setHarioGrounded g
    $ m $ player l) (map (enemyUpdate eT g . enemyStompedCheck p) e) g c

worldGridUpdate:: Level -> Level
worldGridUpdate level@(Level h@(Hario (x,y) s p k (vx,vy) m l co) e g c) = qblockcollide $ blockcollide $ flagcollide $ coincollide level
    where
        collidingFields = map (pointToField g) (getHarioHitBoxCorners h)
        collidingFieldsTop = map (pointToField g) getFieldsTop
        getFieldsTop = case p of
            Small -> [(x,y+9),(x-7,y+9),(x+7,y+9)]
            Big -> [(x,y+18),(x-7.5,y+18),(x+7.5,y+18)]
            Fire -> [(x,y+18),(x-7.5,y+18),(x+7.5,y+18)]
        coincollide :: Level -> Level
        coincollide i = if C `elem` collidingFields
                    then i {player=Hario (x,y) s p k (vx,vy) m l (co+1)
                    ,grid = setFieldInWorldGrid A g (collisionAt C collidingFields (getHarioHitBoxCorners h))}
                    else i
        collisionAt f fs gs = case f `elemIndex` fs of
            Just i -> gs!!i
            Nothing -> (0,0)
        flagcollide :: Level -> Level
        flagcollide i = if X 0 `elem` collidingFields || X 1 `elem` collidingFields || X 2 `elem` collidingFields
                    then i {player=Hario (x,y) Victory p k (vx,vy) m l (co+1)}
                    else i
        blockcollide :: Level -> Level
        blockcollide i = if W 1 `elem` collidingFieldsTop && (p == Big || p == Fire) && vy>0
                    then i {player=Hario (x,y) s p k (vx,-2) m l co,
                    grid = setFieldInWorldGrid A g (collisionAt (W 1) collidingFieldsTop getFieldsTop)}
                    else i
        qblockcollide :: Level -> Level
        qblockcollide i = if Q 0 `elem` collidingFieldsTop && vy > 0
                    then i {player=Hario (x,y) s p k (vx,-2) m l co,
                    grid = setFieldInWorldGrid (Q 1) g (collisionAt (Q 0) collidingFieldsTop getFieldsTop)}
                    else i

setFieldInWorldGrid:: Field -> WorldGrid -> Point -> WorldGrid
setFieldInWorldGrid f g (x,y) = setElem gy g (rowset (g!!gy))
    where gx = if x<0 then 0 else floor (x/16)
          gy = if y>=0 then 0 else floor (y/(-16))
          rowset r = setElem gx r f

setElem::Int -> [a] -> a -> [a]
setElem n xs newElement = take n xs ++ [newElement] ++ drop (n + 1) xs


setHarioGrounded::WorldGrid -> Hario -> Hario
setHarioGrounded w h@(Hario (x,y) s p k v m l co) = Hario (x,y) s p k v (harioGrounded w h) l co

checkFor1Up:: Hario -> Hario
checkFor1Up h@(Hario (x,y) s p k v m l co) | co >= 100 = h{lives = lives h+1,coins = coins h - 100}
                                           | otherwise = h

deathByFalling::WorldGrid -> Hario -> Hario
deathByFalling w h@(Hario (x,y) s p k v m l co) | let
    downBorder = (-1)*(16*fromIntegral (length w))
    in y<downBorder = Hario (x,y) Die p k v m l co
    | otherwise = h

deathByTime::Float-> Hario -> Hario
deathByTime eT h | eT>500 = h {state=Die}
                 | otherwise = h


pointToField::WorldGrid -> (Float,Float)-> Field
pointToField w (px,py) | px<0 || px>=rightBorder = W 0
                       | py>0 || py<=downBorder = A
                       | otherwise = w!!cordToInt (-py)!!cordToInt px
    where
        rightBorder = 16*fromIntegral (length (head w))
        downBorder =(-16)*fromIntegral (length w)
        cordToInt cord = floor (cord/16)

harioGrounded::WorldGrid -> Hario -> Bool
harioGrounded w h@(Hario (x,y) s p k l m) | let
    rightBorder = 16*fromIntegral (length (head w))
    downBorder  =  -16*fromIntegral (length w)
    cordToInt cord = floor (cord/16)
    pointtofield (px,py) | px<0 || px>rightBorder || py<downBorder = W 0
                         | py>0 = A
                         | otherwise = (w!!cordToInt (-py))!!cordToInt px
    fieldSolid f = case f of
            A -> False
            C -> False
            X i -> False
            H -> False
            E _ -> False
            _ -> True
    pointToCheck (px,py) u = case u of
        Small -> [(px,py-9),(px-7,py-9),(px+7,py-9)]
        _ -> [(px,py-18),(px-7.5,py-18),(px+7.5,py-18)]
    in any (fieldSolid . pointToField w) (pointToCheck (x,y) p) = True
    | otherwise = False



tileCollisionCheck:: WorldGrid -> Hario -> Hario
tileCollisionCheck w h@(Hario pos s p d (0,0) g l co) = h
tileCollisionCheck w h@(Hario pos@(x,y) s p d (vx,vy) g l co)
    | any fieldSolid collidingFields = collide
    | otherwise = Hario (x,y) s p d (vx,vy) g l co
    where
        collide
          | not al && ar =  tileCollisionCheck w (Hario (x-4,y) s p d (toZero vx 0.01,toZero vy 0.01) g l co)
          | not ar && al =  tileCollisionCheck w (Hario (x+4,y) s p d (toZero vx 0.01,toZero vy 0.01) g l co)
          | otherwise = tileCollisionCheck w (Hario (x,y) s p d (toZero vx 0.01,toZero vy 0.01) g l co)
        toZero n o
          | n>0 = if n > o then n-o else 0
          | n<0 = if n < -o then n+o else 0
          | otherwise = 0
        corners = getHarioHitBoxCorners (Hario (x+vx,y+vy) s p d (vx,vy) g l co)
        back r = if r>0 then r-1 else r+1
        al = fieldSolid (pointtofield (x-8,snd (last corners))) && not (fieldSolid $ pointtofield (x-7,snd (last corners))) ||
            fieldSolid (pointtofield (x-8,snd (head corners))) &&  not (fieldSolid $ pointtofield (x-7,snd (head corners)))
        ar = fieldSolid (pointtofield (x+8,snd (last corners))) && not (fieldSolid $ pointtofield (x+7,snd (last corners))) ||
            fieldSolid (pointtofield (x+8,snd (head corners))) &&  not (fieldSolid $ pointtofield (x+7,snd (head corners)))
        pointtofield = pointToField w
        collidingFieldsl = map pointtofield (take (div (length corners) 2) corners)
        collidingFieldsr = map pointtofield (drop (div (length corners) 2) corners)
        collidingFields = map pointtofield corners
        fieldSolid f = case f of
            A -> False
            C -> False
            X i -> False
            H -> False
            E _ -> False
            W 8 -> False
            _ -> True

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) world = world { keys = S.insert k (keys world)}
handleInput (EventKey k Up _ _) world = world { keys = S.delete k (keys world)}
handleInput (EventMotion p) w@(StartScreenState {})= w {mousePos = p}
handleInput (EventMotion p) w@(LevelSelectState {})= w {mousePos = p}
handleInput e gstate = gstate
