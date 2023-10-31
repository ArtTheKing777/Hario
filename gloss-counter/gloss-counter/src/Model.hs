-- | This module contains the data types
--   which represent the state of the game
module Model where
import UI
import Graphics.Gloss.Interface.IO.Game ( Point, Key, black, red, Vector, Picture)
import qualified Data.Set as S
import Prelude hiding (Left, Right)
import Fileload (getHarioBmp, getLevel)
import Data.Maybe (isJust)
import Data.Map
import Graphics.Gloss (BitmapData)

data GameState = LevelSelectState { keys::S.Set Key,  elapsedTime::Float,mousePos::(Float,Float),ui::IO [UIElement],loadedAnimations::Map String BitmapData}
              | StartScreenState  { keys::S.Set Key ,  elapsedTime::Float,mousePos::(Float,Float),ui::IO [UIElement],loadedAnimations::Map String BitmapData}
              | LevelPlayingState { keys::S.Set Key,  elapsedTime::Float, 
                                    level::IO Level,loadedAnimations::Map String BitmapData}

initialState :: Map String BitmapData -> GameState
initialState = initialStartScreenState

initialStartScreenState:: Map String BitmapData -> GameState
initialStartScreenState = StartScreenState S.empty 0 (0,0) 
 (  addUIElement (somethingElse getHarioBmp (0.5,0.5) (0,0)) $ 
    addUIElement (button "start" (0.3,0.3) (0,-150) red) (pure []))

initialLevelSelectState:: Map String BitmapData -> GameState
initialLevelSelectState = StartScreenState S.empty 0 (0,0) 
    (   addUIElement (button "2" (0.3,0.3) (0,150) red) $
        addUIElement (button "1" (0.3,0.3) (0,-150) red) (pure []))

initialLevelPlayingState:: Map String BitmapData -> String -> GameState
initialLevelPlayingState m s = LevelPlayingState S.empty 0 (createLevel s) m

    --LevelPlayingState S.empty 0 (Level (Hario(0, 0) Walk Small Left 10) [] [[]])
--initialState = LevelPlayingState S.empty 0 (Level (Hario(0, 0) Idle Small Left (0,0) True) [] [[]])

data PlayerState = Idle | Walk | Jump | Fall | Die | Victory | Swim
    deriving (Eq)

data Looking = Left | Right
    deriving (Eq)

data PlayerPower = Small | Big | Fire
    deriving (Eq)

{-data Hario = Hario { hpos::IO Point, state::PlayerState,
                     power::PlayerPower, direction::Looking, speed::Float} -}

data Hario = Hario { hpos::Point, state::PlayerState,
                     power::PlayerPower, direction::Looking, velocity::Vector, onground::Bool}
data EnemyType = Hoomba | HoopaTroopa | HoopaParaTroopa | Hirrana | RedHirrana | HeepHeep | Hloober | Hakitu | Hiny | HuzzyBeetle | HoolitBill 
                        | HammerBrother | Worm | Howser | HoopaShell | HireBall | Hacid | Hammer  | HakituProjectile

data EnemyState = Alive | Dead

data Enemy = Enemy { point::Point, etype::EnemyType, estate::EnemyState, edirection::Looking, eupdate::Float->Enemy->Enemy }

data Field = W Int -- Wall
            |A -- Air
            |H -- player start position (Hario)
            |C -- Coin
            |X Int -- Finish
            |Q Int -- ?block
            |B Int -- Block
            |P -- platform
            |I Int -- pipe
            |E EnemyType
type Row = [Field]
type WorldGrid = [Row]
data Level = Level{player::Hario, enemies::[Enemy], grid::WorldGrid}

addUIElement ::  IO UIElement -> IO [UIElement] -> IO [UIElement]
addUIElement e l = do
                    uie <- e
                    uies <- l
                    return (uies ++ [uie])


createLevel::String -> IO Level
createLevel s = do
                    sGrid <- getLevel s
                    grid <- createGrid sGrid
                    harioPos <- findHarioPos grid
                    enemyPos <- findEnemyPos grid
                    return (Level (Hario harioPos Idle Small Right (0,0) True) enemyPos grid )

findHarioPos:: [[Field]] -> IO Point
findHarioPos gIO = do 
    g <- pure gIO
    let check c = case c of
            H -> True
            _ -> False
        checkline xp l = case l of
                [] -> Nothing
                (x:xs) -> if check x 
                          then Just xp 
                          else checkline (xp+1) xs
        checkgrid yp g = case g of
                [] -> (0,0)
                (y:ys) -> let xp = checkline 0 y in 
                          case xp of
                          Just x -> (x,yp)
                          Nothing -> checkgrid (yp+1) ys
    return (checkgrid 0 g)

findEnemyPos:: [[Field]] -> IO [Enemy]
findEnemyPos gIO = do
    g <- pure gIO
    let check c = case c of
            E t -> Just t
            c -> Nothing
        checkline xp yp l = case l of
                [] -> []
                (x:xs) -> let t = check x in
                    case t of
                        Just t -> Enemy (xp,yp) t Alive Right genericEnemyUpdate : checkline (xp+1) yp l 
                        Nothing -> checkline (xp+1) yp xs
        checkgrid ypg gi = case gi of
                [] -> []
                (y:ys) -> checkline 0 ypg y ++ checkgrid (ypg+1) ys
    return (checkgrid 0 g)

createGrid::[[Char]] -> IO WorldGrid
createGrid cg = do
    let char c = case c of
            '#' -> W 4
            '.' -> A
            '1' -> H
            'G' -> C
            'H' -> Q 0
            'X' -> X 1
            'I' -> I 4
            'i' -> I 5
            'B' -> I 10
            'b' -> I 11
            'M' -> E Hoomba
            'W' -> E Worm
            c   -> A --if it doesn't know just air
        charline cl = case cl of
            [] -> []
            (x:xs) -> char x : charline xs
        chargrid cgr = case cgr of
            [] -> []
            (y:ys) -> charline y : chargrid ys
    return (chargrid cg)

-- enemy update functions

genericEnemyUpdate :: Float -> Enemy -> Enemy
genericEnemyUpdate eT e = e