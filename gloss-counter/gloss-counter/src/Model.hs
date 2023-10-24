{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

data GameState = LevelSelectState {keys::Int, elapsedTime::Float}
              | StartScreenState  {keys::Int, elapsedTime::Float,mousePos::(Float,Float)}
              | LevelPlayingState {keys::Int, elapsedTime::Float}

initialState :: GameState
initialState = StartScreenState 0 0 (0,0)

