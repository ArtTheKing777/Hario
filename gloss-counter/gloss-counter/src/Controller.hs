{-# LANGUAGE TemplateHaskell #-}

module Controller where

import Model
import qualified Data.Set as S

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import UI (UIElement (Button, SomethingElse), button, hoveredButton)
import GHC.Float (int2Float)
import Hario

-- | Handle one iteration of the game | eT = elaspsedTme
update_ :: Float -> GameState -> IO GameState
update_ eT s@(LevelSelectState k t mp ui a l) = updateUI (t + eT) s
update_ eT s@(StartScreenState k t mp ui a l) = updateUI (t + eT) s
update_ eT s@(LevelPlayingState k t h a l) = step eT s

updateUI :: Float -> GameState -> IO GameState
updateUI eT s@(StartScreenState k t mp ui a l) = do
    uic <- ui
    let news u = case u of
                [] -> []
                (x:xs) -> case x of
                        (Button t s _ p pic) -> if hoveredButton mp x
                                              then button t s p blue : news xs
                                              else button t s p black : news xs
                        (SomethingElse pic)  -> return x : news xs
        newstate = case whichButtonPressed uic mp of
                (Just (Button tx _ _ _ _)) -> (if S.member (MouseButton LeftButton) k
                    then buttonPressedActions tx s
                    else StartScreenState k t mp (sequenceA (news uic)) a l) --lol
                Nothing -> StartScreenState k t mp (sequenceA (news uic)) a l
    return newstate
updateUI eT s = return s

whichButtonPressed:: [UIElement] -> (Float,Float) -> Maybe UIElement
whichButtonPressed [] mp = Nothing
whichButtonPressed (x@(Button t s b p pic) :xs) mp | hoveredButton mp x = Just x
                                                   | otherwise = whichButtonPressed xs mp
whichButtonPressed (x:xs) mp = whichButtonPressed xs mp



buttonPressedActions:: String -> GameState -> GameState
buttonPressedActions b s = case b of
    "start" -> initialLevelSelectState (loadedAnimations s) (loadedLevels s)
    "1" -> initialLevelPlayingState (loadedAnimations s) (loadedLevels s) 1
    "2" -> initialLevelPlayingState (loadedAnimations s) (loadedLevels s) 2
    b -> s

-- | Handle one iteration of the game | eT = elaspsedTme
step :: Float -> GameState -> IO GameState
step eT (LevelSelectState k t p l a s) = return (LevelSelectState k (t + eT) p l a s)
step eT (StartScreenState k t mp l a s) = return (StartScreenState k (t + eT) mp l a s)
step eT p@(LevelPlayingState k t l a s) = updateLevelState eT p

updateLevelState :: Float -> GameState -> IO GameState
updateLevelState eT (LevelPlayingState k t l a s) = do
                                                    let
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
                                                        combine = update' eT l (mover $ movel $ jump_ $ idle)
                                                        in return (LevelPlayingState (keysupdate k) (eT+t) combine a s)

update' :: Float -> Level -> (Hario -> Hario) -> Level
update' eT l@(Level p e g) m = Level (updateHario $ harioGrounded g $ tileCollisionCheck g $ m $ player l) e g

harioGrounded::WorldGrid -> Hario -> Hario
harioGrounded w h@(Hario (x,y) s p k l m) | let
    rightBorder = 16*fromIntegral(length (head w))
    downBorder = -16*fromIntegral(length w)
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
        Small -> [(px,py-9),(px-7.5,py-9),(px+7.5,py-9)]
        _ -> [(px,py-18),(px-7.5,py-18),(px+7.5,py-18)]
    in any (fieldSolid . pointtofield) (pointToCheck (x,y) p) = Hario (x,y) s p k l True
    | otherwise = Hario (x,y) s p k l False
    
    

tileCollisionCheck:: WorldGrid -> Hario -> Hario
tileCollisionCheck w h@(Hario pos s p d (0,0) g) = h
tileCollisionCheck w h@(Hario pos@(x,y) s p d (vx,vy) g) | let
    corners = getHarioHitBoxCorners (Hario (x+vx,y+vy) s p d (vx,vy) g)
    cordToInt cord = floor (cord/16)
    rightBorder = 16*fromIntegral(length (head w))
    downBorder = -16*fromIntegral(length w)
    pointtofield (px,py) | px<0 || px>rightBorder || py<downBorder = W 0
                         | py>0 = A
                         | otherwise = (w!!cordToInt (-py))!!cordToInt px
    collidingFields = map pointtofield corners
    fieldSolid f = case f of
            A -> False
            C -> False
            X i -> False
            H -> False
            E _ -> False
            _ -> True
    in any fieldSolid collidingFields = tileCollisionCheck w (Hario pos s p d (toZero vx 0.01,toZero vy 0.01) g)
    | otherwise = h
    where
        toZero n o
          | n>0 = if n > o then n-o else 0
          | n<0 = if n < -o then n+o else 0
          | otherwise = 0



-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) world = world { keys = S.insert k (keys world)}
handleInput (EventKey k Up _ _) world = world { keys = S.delete k (keys world)}
handleInput (EventMotion p) w@(StartScreenState {})= w {mousePos = p}
handleInput e gstate = gstate