-- | This module contains the data types
--   which represent the state of the game
module Model where
import UI
import Graphics.Gloss.Interface.IO.Game ( Point, Key, black, red )
import qualified Data.Set as S
import Prelude hiding (Left, Right)
import Fileload (getHarioBmp)

data GameState = LevelSelectState { keys::S.Set Key,  elapsedTime::Float}
              | StartScreenState  { keys::S.Set Key ,  elapsedTime::Float,mousePos::(Float,Float),ui::IO [UIElement]}
              | LevelPlayingState { keys::S.Set Key,  elapsedTime::Float, 
                                    level::Level}

initialState :: GameState
initialState = initialStartScreenState

initialStartScreenState:: GameState
initialStartScreenState = StartScreenState S.empty 0 (0,0) 
 (  addUIElement (somethingElse getHarioBmp (0.5,0.5) (0,0)) $ 
    addUIElement (button "start" (0.3,0.3) (0,-150) red) (pure []))


    --LevelPlayingState S.empty 0 (Level (Hario(0, 0) Walk Small Left 10) [] [[]])

data PlayerState = Idle | Walk | Jump | Fall | Die | Victory | Swim
    deriving (Eq)
data Looking = Left | Right
    deriving (Eq)
data PlayerPower = Small | Big | Fire
    deriving (Eq)
data Hario = Hario { hpos::Point, state::PlayerState,
                     power::PlayerPower, direction::Looking, speed::Float}
data EnemyType = Hoomba | HoopaTroopa | HoopaParaTroopa | Hirrana | RedHirrana | HeepHeep | Hloober | Hakitu | Hiny | HuzzyBeetle | HoolitBill 
                        | HammerBrother | Worm | Howser | HoopaShell | HireBall | Hacid | Hammer  | HakituProjectile
data EnemyState = Alive | Dead
data Enemy = Enemy { point::Point, etype::EnemyType, estate::EnemyState, edirection::Looking }
data Field = W -- Wall
            |A -- Air
            |H -- player start position (Hario)
            |C -- Coin
            |X -- Finish
            |Q -- ?block
            |B -- Block
            |P -- platform
type Row = [Field]
type WorldGrid = [Row]
data Level = Level{player::Hario, enemies::[Enemy], grid::WorldGrid}

addUIElement ::  IO UIElement -> IO [UIElement] -> IO [UIElement]
addUIElement e l = do
                    uie <- e
                    uies <- l
                    return (uies ++ [uie])

                    