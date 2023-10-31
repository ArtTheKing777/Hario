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
update_ eT s@(LevelSelectState k t mp ui a) = updateUI (t + eT) s
update_ eT s@(StartScreenState k t mp ui a) = updateUI (t + eT) s
update_ eT s@(LevelPlayingState k t h a) = step eT s

updateUI :: Float -> GameState -> IO GameState
updateUI eT s@(StartScreenState k t mp ui a) = do
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
                    else StartScreenState k t mp (sequenceA (news uic)) a) --lol
                Nothing -> StartScreenState k t mp (sequenceA (news uic)) a
    return newstate
updateUI eT s = return s

whichButtonPressed:: [UIElement] -> (Float,Float) -> Maybe UIElement
whichButtonPressed [] mp = Nothing
whichButtonPressed (x@(Button t s b p pic) :xs) mp | hoveredButton mp x = Just x
                                                   | otherwise = whichButtonPressed xs mp
whichButtonPressed (x:xs) mp = whichButtonPressed xs mp
    


buttonPressedActions:: String -> GameState -> GameState
buttonPressedActions b s = case b of
    "start" -> initialLevelSelectState (loadedAnimations s)
    "1" -> initialLevelPlayingState (loadedAnimations s) b
    "2" -> initialLevelPlayingState (loadedAnimations s) b
    b -> s

-- | Handle one iteration of the game | eT = elaspsedTme
step :: Float -> GameState -> IO GameState
step eT (LevelSelectState k t p l a) = return (LevelSelectState k (t + eT) p l a)
step eT (StartScreenState k t mp l a) = return (StartScreenState k (t + eT) mp l a)
step eT s@(LevelPlayingState k t l a) = updateLevelState eT s

updateLevelState :: Float -> GameState -> IO GameState
updateLevelState eT (LevelPlayingState k t l a) = do
                                                    level <- l
                                                    if S.member (Char 'd')k 
                                                        then return (LevelPlayingState k (t + eT) (pure(update' eT "Left" level)) a)
                                                    else if S.member (Char 'a')k 
                                                        then return (LevelPlayingState k (t + eT) (pure(update' eT "Right" level)) a)
                                                    else if S.member (SpecialKey KeySpace)k 
                                                        then return (LevelPlayingState k (t + eT) (pure(update' eT "Jump" level)) a)
                                                    else return (LevelPlayingState k (t + eT) (pure(update' eT "" level)) a)

update' :: Float -> String -> Level -> Level
update' eT "Left" l@(Level p e g)  = Level (updateHario(moveLeft (player l))) e g
update' eT "Right" l@(Level p e g) = Level (updateHario(moveRight (player l))) e g
update' eT "Jump" l@(Level p e g)  = Level (updateHario(jump (player l))) e g
update' eT _ l@(Level p e g)       = Level (updateHario (player l)) e g

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) world = world { keys = S.insert k (keys world)}
handleInput (EventKey k Up _ _) world = world { keys = S.delete k (keys world)}
handleInput (EventMotion p) w@(StartScreenState {})= w {mousePos = p}
handleInput e gstate = gstate