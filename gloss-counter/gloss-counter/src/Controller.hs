{-# LANGUAGE TemplateHaskell #-}

module Controller where

import Model
import qualified Data.Set as S

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import UI (UIElement (Button, SomethingElse), button, hoveredButton)
import GHC.Float (int2Float)

-- | Handle one iteration of the game | eT = elaspsedTme
update :: Float -> GameState -> IO GameState
update eT s@(LevelSelectState k t mp ui) = updateUI (t + eT) s
update eT s@(StartScreenState k t mp ui) = updateUI (t + eT) s
update eT s@(LevelPlayingState k t h) = step eT s

updateUI :: Float -> GameState -> IO GameState
updateUI eT s@(StartScreenState k t mp ui) = do
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
                    else StartScreenState k t mp (sequenceA (news uic))) --lol
                Nothing -> StartScreenState k t mp (sequenceA (news uic))
    return newstate
updateUI eT s = return s

whichButtonPressed:: [UIElement] -> (Float,Float) -> Maybe UIElement
whichButtonPressed [] mp = Nothing
whichButtonPressed (x@(Button t s b p pic) :xs) mp | hoveredButton mp x = Just x
                                                   | otherwise = whichButtonPressed xs mp
whichButtonPressed (x:xs) mp = whichButtonPressed xs mp
    


buttonPressedActions:: String -> GameState -> GameState
buttonPressedActions b s = case b of
    "start" -> initialLevelSelectState
    "1" -> initialLevelPlayingState b
    b -> s
import Hario

-- | Handle one iteration of the game | eT = elaspsedTme
step :: Float -> GameState -> IO GameState
step eT (LevelSelectState k t ) = return (LevelSelectState k (t + eT))
step eT (StartScreenState k t mp) = return (StartScreenState k (t + eT) mp)
step eT (LevelPlayingState k t l)   | S.member (Char 'd')k = return (LevelPlayingState k (t + eT) l {player = updateHario(moveRight (player l))})
                                    | S.member (Char 'a')k = return (LevelPlayingState k (t + eT) l {player = updateHario(moveLeft (player l))})
                                    | S.member (SpecialKey KeySpace)k = return (LevelPlayingState k (t + eT) l {player = updateHario(jump (player l))})
                                    | otherwise = return(LevelPlayingState k (t + eT) l {player = updateHario(idle (player l))} )

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) world = world { keys = S.insert k (keys world)}
handleInput (EventKey k Up _ _) world = world { keys = S.delete k (keys world)}
handleInput (EventMotion p) w@(StartScreenState {})= w {mousePos = p}
handleInput e gstate = gstate