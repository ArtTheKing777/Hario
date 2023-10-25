-- | This module contains the data types
--   which represent the state of the game
module Model where
import Graphics.Gloss.Interface.IO.Game ( Point, Key )
import qualified Data.Set as S

data GameState = LevelSelectState { keys::S.Set Key,  elapsedTime::Float}
              | StartScreenState  { keys::S.Set Key ,  elapsedTime::Float,mousePos::(Float,Float)}
              | LevelPlayingState { keys::S.Set Key,  elapsedTime::Float, 
                                    hario::Hario}

initialState :: GameState
initialState = StartScreenState S.empty 0 (0,0)

data PlayerState = Idle | Walk | Jump | Fall | Die | Victory
data Looking = Left | Right
data PlayerPower = Small | Big | Fire
    deriving (Eq)
data Hario = Hario { hpos::Point, state::PlayerState,
                     power::PlayerPower, direction::Looking}