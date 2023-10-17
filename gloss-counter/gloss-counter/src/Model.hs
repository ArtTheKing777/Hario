{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
-- | This module contains the data types
--   which represent the state of the game
module Model where

data GameState = LevelSelectState {keys::Int}
              | StartScreenState  {}
              | LevelPlayingState {}

initialState :: GameState
initialState = StartScreenState

