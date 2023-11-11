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
import Control.Monad.State

data GameState = LevelSelectState { keys::S.Set Key,  elapsedTime::Float,mousePos::(Float,Float),ui::[UIElement],loadedAnimations::Map String BitmapData,loadedLevels::[[[Char]]]}
              | StartScreenState  { keys::S.Set Key ,  elapsedTime::Float,mousePos::(Float,Float),ui::[UIElement],loadedAnimations::Map String BitmapData,loadedLevels::[[[Char]]]}
              | LevelPlayingState { keys::S.Set Key,  elapsedTime::Float,
                                    level::Level,loadedAnimations::Map String BitmapData,loadedLevels::[[[Char]]]}

initialState :: Map String BitmapData -> [[[Char]]] -> GameState
initialState = initialStartScreenState


initialStartScreenState:: Map String BitmapData -> [[[Char]]] -> GameState
initialStartScreenState dic = StartScreenState S.empty 0 (0,0)
 [somethingElse (dic!"harioBmp") (0.5,0.5) (0,0),
 button "start" (0.3,0.3) (0,-150) red (dic!"textBoxBmp")] dic

initialLevelSelectState:: Map String BitmapData -> [[[Char]]] -> GameState
initialLevelSelectState dic l = LevelSelectState S.empty 0 (0,0)
    [levelbutton "2" (0.3,0.3) (0,150) red [dic!"textBoxBmp",dic!"textBoxBmpNo"] (levelUnlucked 2) 
    ,levelbutton "1" (0.3,0.3) (0,-150) red [dic!"textBoxBmp",dic!"textBoxBmpNo"] (levelUnlucked 1)] dic l
    where levelUnlucked i = cl+1>=i
          cl = read (head l!!2) :: Int


initialLevelPlayingState:: Map String BitmapData -> [[[Char]]] -> Int -> GameState
initialLevelPlayingState m l i = LevelPlayingState S.empty 0 (createLevel i l) m l

    --LevelPlayingState S.empty 0 (Level (Hario(0, 0) Walk Small Left 10) [] [[]])
--initialState = LevelPlayingState S.empty 0 (Level (Hario(0, 0) Idle Small Left (0,0) True) [] [[]])

data PlayerState = Idle | Walk | Jump | Fall | Die | Victory | Swim
    deriving (Eq)

data Looking = Left | Right
    deriving (Eq)

data PlayerPower = Small | Big | Fire
    deriving(Eq,Show)

{-data Hario = Hario { hpos::IO Point, state::PlayerState,
                     power::PlayerPower, direction::Looking, speed::Float} -}

data Hario = Hario { hpos::Point, state::PlayerState,
                     power::PlayerPower, direction::Looking, velocity::Vector, onground::Bool,lives::Int,coins::Int}
data EnemyType = Hoomba | HoopaTroopa | HoopaParaTroopa | Hirrana | RedHirrana | HeepHeep | Hloober | Hakitu | Hiny | HuzzyBeetle | HoolitBill
                        | HammerBrother | Worm | Howser | HoopaShell | HireBall | Hacid | Hammer  | HakituProjectile deriving(Eq,Show)

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
            deriving(Eq,Show)


type Row = [Field]
type WorldGrid = [Row]
data Level = Level{player::Hario, enemies::[Enemy], grid::WorldGrid, currentLevel::Int}


createLevel::Int -> [[[Char]]] -> Level
createLevel i s = do
                    let g = s!!i
                        grid = createGrid g
                        harioPos = findHarioPos grid
                        enemyPos = findEnemyPos grid
                    Level (loadHario (head s) harioPos) enemyPos grid i

reCreateLevel::Int -> [[[Char]]] -> Hario -> Level
reCreateLevel i s h@(Hario (x,y) Idle p o v m l c) = do
                    let g = s!!i
                        grid = createGrid g
                        harioPos = findHarioPos grid
                        enemyPos = findEnemyPos grid
                    Level (Hario harioPos Idle Small Right (0,0) False l c) enemyPos grid i

loadHario::[[Char]] -> Point -> Hario
loadHario c p = Hario p Idle (harioType $ c!!1) Right (0,0) False lives coins
    where lives = read (head c) :: Int
          harioType t = case t of
            "Small" -> Small
            "Big" -> Big
            _ -> Fire
          coins = read (c!!3) :: Int

createHarioSave:: Hario -> Int -> [[Char]] -> [[Char]]
createHarioSave h i b = [l,harioType,lev,c]
    where l = show $ lives h
          harioType = case power h of
             Small -> "Small"
             Big -> "Big"
             _ -> "Fire"
          c =  show $ coins h
          clev =  read (b!!2) :: Int
          lev | clev<i = show i
              | otherwise = show clev

createEmptyHarioSave:: [[Char]]
createEmptyHarioSave = ["5","Small","0","0"]

findHarioPos:: [[Field]] -> Point
findHarioPos gIO = do
    let g = gIO
        check c = case c of
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
                          Just x -> (4*16,-1*16)
                          Nothing -> checkgrid (yp+1) ys
    checkgrid 0 g

findEnemyPos:: [[Field]] -> [Enemy]
findEnemyPos gIO = do
    let g = gIO
        check c = case c of
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
    checkgrid 0 g

createGrid::[[Char]] -> WorldGrid
createGrid cg = do
    let char c = case c of
            '#' -> W 4
            '*' -> W 1
            '-' -> W 8
            '.' -> A
            '1' -> H
            'G' -> C
            'H' -> Q 0
            '0' -> Q 1
            'X' -> X 2
            ',' -> X 0
            '|' -> X 1
            '/' -> X 3
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
    chargrid cg



-- enemy update functions

genericEnemyUpdate :: Float -> Enemy -> Enemy
genericEnemyUpdate eT e = e