
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game | eT = elaspsedTme
step :: Float -> GameState -> IO GameState
step eT (LevelSelectState k t ) = return (LevelSelectState k (t + eT))
step eT (StartScreenState k t ) = return (StartScreenState k (t + eT))
step eT (LevelPlayingState k t ) = return (LevelPlayingState k (t + eT))


-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (handleInput e gstate)

handleInput :: Event -> GameState -> GameState
handleInput e gstate = gstate