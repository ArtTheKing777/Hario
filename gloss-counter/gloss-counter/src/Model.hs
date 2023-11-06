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

data GameState = LevelSelectState { keys::S.Set Key,  elapsedTime::Float,mousePos::(Float,Float),ui::IO [UIElement],loadedAnimations::Map String BitmapData,loadedLevels::[[[Char]]]}
              | StartScreenState  { keys::S.Set Key ,  elapsedTime::Float,mousePos::(Float,Float),ui::IO [UIElement],loadedAnimations::Map String BitmapData,loadedLevels::[[[Char]]]}
              | LevelPlayingState { keys::S.Set Key,  elapsedTime::Float,
                                    level::Level,loadedAnimations::Map String BitmapData,loadedLevels::[[[Char]]]}

initialState :: Map String BitmapData -> [[[Char]]] -> GameState
initialState = initialStartScreenState


initialStartScreenState:: Map String BitmapData -> [[[Char]]] -> GameState
initialStartScreenState = StartScreenState S.empty 0 (0,0)
 (  addUIElement (somethingElse getHarioBmp (0.5,0.5) (0,0)) $
    addUIElement (button "start" (0.3,0.3) (0,-150) red) (pure []))

initialLevelSelectState:: Map String BitmapData -> [[[Char]]] -> GameState
initialLevelSelectState = StartScreenState S.empty 0 (0,0)
    (   addUIElement (button "2" (0.3,0.3) (0,150) red) $
        addUIElement (button "1" (0.3,0.3) (0,-150) red) (pure []))

initialLevelPlayingState:: Map String BitmapData -> [[[Char]]] -> Int -> GameState
initialLevelPlayingState m l i = LevelPlayingState S.empty 0 (createLevel (l!!(i-1))) m l

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

data EnemyState = EIdle | EWalk | EAttack | EDie | EDead

data Enemy = Enemy { point::Point, etype::EnemyType, estate::EnemyState, edirection::Looking}

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


createLevel::[[Char]] -> Level
createLevel s = do
                    let grid = createGrid s
                        harioPos = findHarioPos grid
                        enemyPos = findEnemyPos grid
                    Level (Hario harioPos Idle Small Right (0,0) False) enemyPos grid

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
findEnemyPos g = do
        let check c = case c of
                E t -> Just t
                c -> Nothing
            checkline xp yp l = case l of
                [] -> []
                (x:xs) -> let t = check x in
                    case t of
                        Just t -> Enemy ((16*xp),-16*yp) t EWalk Right : checkline (xp+1) yp xs
                        Nothing -> checkline (xp+1) yp xs
            checkgrid ypg gi = case gi of
                [] -> []
                (y:ys) -> checkline 0 ypg y ++ checkgrid (ypg+1) ys
        checkgrid 0 g

createGrid::[[Char]] -> WorldGrid
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
    chargrid cg

-- enemy functions

getEnemySize :: Enemy -> Point
getEnemySize (Enemy _ Hoomba _ _) = (16, 16)
getEnemySize (Enemy _ HoopaTroopa _ _) = (16, 23)
getEnemySize (Enemy _ HoopaParaTroopa _ _) = (16, 23)
getEnemySize (Enemy _ HoopaShell _ _) = (16, 15)
getEnemySize (Enemy _ Hirrana _ _) = (16, 23)
getEnemySize (Enemy _ RedHirrana _ _) = (16, 23)
getEnemySize (Enemy _ HeepHeep _ _) = (16, 17)
getEnemySize (Enemy _ Hloober _ _) = (16, 26)
getEnemySize (Enemy _ Hakitu _ _) = (16, 24)
getEnemySize (Enemy _ HakituProjectile _ _) = (16, 16)
getEnemySize (Enemy _ Hiny _ _) = (16, 15)
getEnemySize (Enemy _ HuzzyBeetle _ _) = (16, 15)
getEnemySize (Enemy _ HoolitBill _ _) = (16, 16)
getEnemySize (Enemy _ HammerBrother _ _) = (16, 24)
getEnemySize (Enemy _ HireBall _ _) = (8,8)
getEnemySize (Enemy _ Hacid _ _) = (8,8)
getEnemySize (Enemy _ Hammer _ _) = (16,16)

genericEnemyUpdate :: Float -> Enemy -> Enemy
genericEnemyUpdate eT e = e

canEnemyMoveForward :: Enemy -> WorldGrid -> Bool
canEnemyMoveForward e g = not (willEnemyHitWall e g || willEnemyFall e g)

willEnemyFall :: Enemy -> WorldGrid -> Bool
willEnemyFall e@(Enemy (x,y) _ _ Left) g = case pointToField (x-0.5-(fst(getEnemySize e)/2), y-(snd(getEnemySize e)/2)-8) g of
                                                    A -> True
                                                    _ -> False
willEnemyFall e@(Enemy (x,y) _ _ Right) g = case pointToField (x+(fst(getEnemySize e)/2)+1, y-(snd(getEnemySize e)/2)-8) g of
                                                    A -> True
                                                    _ -> False

willEnemyHitWall :: Enemy -> WorldGrid -> Bool
willEnemyHitWall e@(Enemy (x,y) _ _ Left) g = case pointToField (x-0.5-(fst(getEnemySize e)/2), y) g of  --(I || W || Q || B)
                                                    A -> False
                                                    C -> False
                                                    X i -> False
                                                    H -> False
                                                    E _ -> False
                                                    _ -> True
willEnemyHitWall e@(Enemy (x,y) _ _ Right) g = case pointToField (x+0.5+(fst(getEnemySize e)/2), y) g of  --(I || W || Q || B)
                                                    A -> False
                                                    C -> False
                                                    X i -> False
                                                    H -> False
                                                    E _ -> False
                                                    _ -> True

pointToField:: (Float,Float)-> WorldGrid -> Field
pointToField (px,py) w | px<0 || px>=rightBorder = W 0
                       | py>0 || py<=downBorder = A
                       | otherwise = w!!cordToInt (-py)!!cordToInt px
    where
        rightBorder = 16*fromIntegral (length (head w))
        downBorder =(-16)*fromIntegral (length w)
        cordToInt cord = floor (cord/16)
--
enemyUpdate :: Float -> WorldGrid -> Enemy -> Enemy
enemyUpdate eT g e@(Enemy (x,y) Hoomba s Left)  | canEnemyMoveForward e g = e {point = (x-0.5, y)}
                                                | otherwise = e {edirection = Right} {point = (x, y)}
enemyUpdate eT g e@(Enemy (x,y) Hoomba s Right) | canEnemyMoveForward e g = e {point = (x+0.5, y)}
                                                | otherwise = e {edirection = Left} {point = (x, y)}
enemyUpdate eT g e = genericEnemyUpdate eT e

