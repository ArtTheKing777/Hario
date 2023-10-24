{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss (Point)

data GameState = LevelSelectState { keys::Int,  elapsedTime::Float}
              | StartScreenState  { keys::Int,  elapsedTime::Float,mousePos::(Float,Float)}
              | LevelPlayingState { keys::Int,  elapsedTime::Float, 
                                    hario::Hario}

initialState :: GameState
initialState = StartScreenState 0 0 (0,0)

data PlayerState = Idle | Walk | Jump | Fall | Die | Victory
data Looking = Left | Right
data PlayerPower = Small | Big | Fire
data Hario = Hario { hpos::Point, state::PlayerState,
                     power::PlayerPower, direction::Looking}