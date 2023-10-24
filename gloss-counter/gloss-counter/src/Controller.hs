
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game | eT = elaspsedTme
step :: Float -> GameState -> IO GameState
step eT (LevelSelectState k t ) = return (LevelSelectState k (t + eT))
step eT (StartScreenState k t mp) = return (StartScreenState k (t + eT) mp)
step eT (LevelPlayingState k t h) = return (LevelPlayingState k (t + eT) h)


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput (EventMotion p) w@(StartScreenState {})= w {mousePos = p}
handleInput e gstate = gstate