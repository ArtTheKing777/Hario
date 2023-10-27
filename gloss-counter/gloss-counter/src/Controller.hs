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
update eT (LevelSelectState k t ) = return (LevelSelectState k (t + eT))
update eT s@(StartScreenState k t mp ui) = updateUI eT s
update eT (LevelPlayingState k t h) = return (LevelPlayingState k (t + eT) h)

updateUI :: Float -> GameState -> IO GameState
updateUI eT s@(StartScreenState k t mp ui) = do
    uic <- ui
    let news u = case u of
                [] -> []
                (x:xs) -> case x of
                        (Button t s _ p pic) -> if hoveredButton mp x
                                              then button t s p blue : news xs
                                              else button t s p black : news xs
                        (SomethingElse pic)-> return x : news xs
    return (StartScreenState k t mp (sequenceA (news uic))) --lol



-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput (EventKey k Down _ _) world = world { keys = S.insert k (keys world)}
handleInput (EventKey k Up _ _) world = world { keys = S.delete k (keys world)}
handleInput (EventMotion p) w@(StartScreenState {})= w {mousePos = p}
handleInput e gstate = gstate