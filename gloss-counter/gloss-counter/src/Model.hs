-- | This module contains the data types
--   which represent the state of the game
module Model where
import UI
import Graphics.Gloss.Interface.IO.Game ( Point, Key, black, red )
import qualified Data.Set as S
import Prelude hiding (Left, Right)
import Fileload (getHarioBmp, getLevel)
import Data.Maybe (isJust)

data GameState = LevelSelectState { keys::S.Set Key,  elapsedTime::Float,mousePos::(Float,Float),ui::IO [UIElement]}
              | StartScreenState  { keys::S.Set Key ,  elapsedTime::Float,mousePos::(Float,Float),ui::IO [UIElement]}
              | LevelPlayingState { keys::S.Set Key,  elapsedTime::Float, 
                                    level::Level}

initialState :: GameState
initialState = initialStartScreenState

initialStartScreenState:: GameState
initialStartScreenState = StartScreenState S.empty 0 (0,0) 
 (  addUIElement (somethingElse getHarioBmp (0.5,0.5) (0,0)) $ 
    addUIElement (button "start" (0.3,0.3) (0,-150) red) (pure []))

initialLevelSelectState:: GameState
initialLevelSelectState = StartScreenState S.empty 0 (0,0) 
 (addUIElement (button "1" (0.3,0.3) (0,-150) red) (pure []))

initialLevelPlayingState:: String -> GameState
initialLevelPlayingState s = LevelPlayingState S.empty 0 (createLevel s)

    --LevelPlayingState S.empty 0 (Level (Hario(0, 0) Walk Small Left 10) [] [[]])

data PlayerState = Idle | Walk | Jump | Fall | Die | Victory | Swim
    deriving (Eq)

data Looking = Left | Right
    deriving (Eq)

data PlayerPower = Small | Big | Fire
    deriving (Eq)

data Hario = Hario { hpos::IO Point, state::PlayerState,
                     power::PlayerPower, direction::Looking, speed::Float}

data EnemyType = Hoomba | HoopaTroopa | HoopaParaTroopa | Hirrana | RedHirrana | HeepHeep | Hloober | Hakitu | Hiny | HuzzyBeetle | HoolitBill 
                        | HammerBrother | Worm | Howser | HoopaShell | HireBall | Hacid | Hammer  | HakituProjectile

data EnemyState = Alive | Dead

data Enemy = Enemy { point::Point, etype::EnemyType, estate::EnemyState, edirection::Looking }

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
data Level = Level{player::Hario, enemies::IO [Enemy], grid::IO WorldGrid}

addUIElement ::  IO UIElement -> IO [UIElement] -> IO [UIElement]
addUIElement e l = do
                    uie <- e
                    uies <- l
                    return (uies ++ [uie])

createLevel::String -> Level
createLevel s = Level (Hario (findHarioPos grid) Idle Small Right 10)  (findEnemyPos grid) grid
    where sGrid = getLevel s
          grid = createGrid sGrid

findHarioPos:: IO [[Field]] -> IO Point
findHarioPos gIO = do 
    g <- gIO
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

findEnemyPos:: IO [[Field]] -> IO [Enemy]
findEnemyPos gIO = do
    g <- gIO
    let check c = case c of
            E t -> Just t
            c -> Nothing
        checkline xp l = case l of
                [] -> Nothing
                (x:xs) -> let t = check x in
                    case t of
                        Just t -> Just(xp,t)
                        Nothing -> checkline (xp+1) xs
        checkgrid yp g = case g of
                [] -> []
                (y:ys) -> let xp = checkline 0 y in 
                    case xp of
                        Just xt -> Enemy (fst xt,yp) (snd xt) Alive Right : checkgrid yp g
                        Nothing -> checkgrid (yp+1) ys
    return (checkgrid 0 g)

createGrid::IO [[Char]] -> IO WorldGrid
createGrid cgio = do
    cg <- cgio
    let char c = case c of
            '#' -> W 0
            '.' -> A
            '1' -> H
            'G' -> C
            'H' -> Q 3
            'X' -> X 1
            'I' -> I 4
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