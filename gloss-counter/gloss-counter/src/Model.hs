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
import GHC.Float

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
initialLevelPlayingState m l i = LevelPlayingState S.empty 1 (createLevel i l) m l


addUIElement ::  IO UIElement -> IO [UIElement] -> IO [UIElement]
addUIElement e l = do
                    uie <- e
                    uies <- l
                    return (uies ++ [uie])

data PlayerState = Idle | Walk | Jump | Fall | Die | Victory | Swim
    deriving (Eq)

data Looking = Left | Right
    deriving (Eq)

data PlayerPower = Small | Big | Fire
    deriving (Eq, Show)

data Hario = Hario { hpos::Point, state::PlayerState,
                     power::PlayerPower, direction::Looking, velocity::Vector, onground::Bool,lives::Int,coins::Int}
data EnemyType = Hoomba | HoopaTroopa | HoopaParaTroopa | Hirrana | RedHirrana | HeepHeep | Hloober | Hakitu | Hiny | HuzzyBeetle | HoolitBill
                        | HammerBrother | Worm Int | Howser | HoopaShell Int | HoopaShellP Int | HireBall | Hacid Int | Hammer  | HakituProjectile
                        | Hushroom | HireFlower deriving(Eq,Show)

data EnemyState = EIdle | EWalk | EAttack | EDie | EDead
    deriving (Eq)

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
            'W' -> E (Worm 0)
            'F' -> E HireFlower
            'R' -> E Hushroom
            c   -> A --if it doesn't know just air
        charline cl = case cl of
            [] -> []
            (x:xs) -> char x : charline xs
        chargrid cgr = case cgr of
            [] -> []
            (y:ys) -> charline y : chargrid ys
    chargrid cg

gravity :: Vector -> Vector
gravity (x,y) = (x,y-0.52)

fgravity :: Vector -> Vector
fgravity (x,y) = (x,y-1)

-- enemy functions

getEnemySize :: Enemy -> Point
getEnemySize (Enemy _ Hoomba _ _) = (16, 16)
getEnemySize (Enemy _ HoopaTroopa _ _) = (16, 23)
getEnemySize (Enemy _ HoopaParaTroopa _ _) = (16, 23)
getEnemySize (Enemy _ (HoopaShell _) _ _) = (16, 15)
getEnemySize (Enemy _ (HoopaShellP _) _ _) = (16, 15)
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
getEnemySize (Enemy _ (Hacid _) _ _) = (8,8)
getEnemySize (Enemy _ Hammer _ _) = (16,16)
getEnemySize (Enemy _ (Worm _) _ _) = (16,16)
getEnemySize (Enemy _ HireFlower _ _) = (16,16)
getEnemySize (Enemy _ Hushroom _ _) = (16,16)

canEnemyMoveForward :: Enemy -> WorldGrid -> Bool
canEnemyMoveForward e g = not (willEnemyHitWall e g || willEnemyFall e g)

willEnemyFall :: Enemy -> WorldGrid -> Bool
willEnemyFall e@(Enemy (x,y) _ _ Left) g = case pointToField (x-0.5-(fst (getEnemySize e)/2), y-(snd (getEnemySize e)/2)-8) g of
                                                    A -> True
                                                    _ -> False
willEnemyFall e@(Enemy (x,y) _ _ Right) g = case pointToField (x+0.5+(fst (getEnemySize e)/2), y-(snd (getEnemySize e)/2)-8) g of
                                                    A -> True
                                                    _ -> False

willEnemyHitWall :: Enemy -> WorldGrid -> Bool
willEnemyHitWall e@(Enemy (x,y) _ _ Left) g = case pointToField (x-0.5-(fst (getEnemySize e)/2), y) g of  --(I || W || Q || B)
                                                    A -> False
                                                    C -> False
                                                    X i -> False
                                                    H -> False
                                                    E _ -> False
                                                    _ -> True
willEnemyHitWall e@(Enemy (x,y) _ _ Right) g = case pointToField (x+0.5+(fst (getEnemySize e)/2), y) g of  --(I || W || Q || B)
                                                    A -> False
                                                    C -> False
                                                    X i -> False
                                                    H -> False
                                                    E _ -> False
                                                    _ -> True

pointToField:: (Float,Float)-> WorldGrid -> Field
pointToField (px,py) w | px<=0 || px>=rightBorder = W 0
                       | py>=0 || py<=downBorder = A
                       | otherwise = w!!cordToInt (-py)!!cordToInt px
    where
        rightBorder = 16*fromIntegral (length (head w)) - 8
        downBorder =(-16)*fromIntegral (length w)
        cordToInt cord = floor (cord/16)

enemyUpdate :: Float -> WorldGrid -> Hario -> [Enemy] -> [Enemy]
enemyUpdate _ _ _ []                                       = []
enemyUpdate eT g h (e@(Enemy p t EDead d):es)              = e : enemyUpdate eT g h es
enemyUpdate eT g h (e@(Enemy p HoopaTroopa EDie d):es)     = Enemy p (HoopaShell 0) EIdle d : enemyUpdate eT g h es
enemyUpdate eT g h (e@(Enemy p HoopaParaTroopa EDie d):es) = Enemy p (HoopaShellP 0) EIdle d : enemyUpdate eT g h es
enemyUpdate eT g h (e@(Enemy p (Hacid t) EDie d):es)       | t >= 5 = Enemy p (Hacid 0) EDead d : enemyUpdate eT g h es
                                                           | otherwise = Enemy p (Hacid (t+1)) EDie d : enemyUpdate eT g h es
enemyUpdate eT g h (e@(Enemy p@(x,y) t EDie d):es)         | y <= (-16)*int2Float(length g) = Enemy (x,y) t EDead d : enemyUpdate eT g h es
                                                           | otherwise = Enemy (fgravity p) t EDie d : enemyUpdate eT g h es
enemyUpdate eT g h (e@(Enemy p@(x,y)(Worm t) EAttack d):es)| t >= 20 = Enemy p (Worm 0) EWalk d : enemyUpdate eT g h (Enemy p (Hacid 0) EWalk d:es)
                                                           | otherwise = Enemy p (Worm (t+1)) EAttack d : enemyUpdate eT g h es
enemyUpdate eT g h (e@(Enemy p@(x,y)(Worm t) s d):es)      | inProximity (5*16) e h && (s /= EAttack) = Enemy p (Worm 0) EAttack (dirCheck e h) : enemyUpdate eT g h es
                                                           | otherwise = genericEnemyUpdate eT g e : enemyUpdate eT g h es
enemyUpdate eT g h (e@(Enemy p@(x,y) (Hacid _) EWalk d):es)| willEnemyHitWall e g = Enemy p (Hacid 0) EDie d : enemyUpdate eT g h es
                                                           | d == Left = Enemy (x-2,y) (Hacid 0) EWalk d : enemyUpdate eT g h es
                                                           | otherwise = Enemy (x+2,y) (Hacid 0) EWalk d : enemyUpdate eT g h es
enemyUpdate eT g h (e@(Enemy _ Hushroom _ _):es)           = hushroomUpdate eT g e : enemyUpdate eT g h es
enemyUpdate eT g h (e@(Enemy (x,y) HireFlower s _):es)     = e : enemyUpdate eT g h es
enemyUpdate eT g h (e:es)                                  = genericEnemyUpdate eT g e : enemyUpdate eT g h es
 
genericEnemyUpdate :: Float -> WorldGrid -> Enemy -> Enemy
genericEnemyUpdate eT g e@(Enemy (x,y) t s Left)    | canEnemyMoveForward e g = e {point = (x-0.5, y)}
                                                    | otherwise = e {edirection = Right} {point = (x, y)}
genericEnemyUpdate eT g e@(Enemy (x,y) t s Right)   | canEnemyMoveForward e g = e {point = (x+0.5, y)}
                                                    | otherwise = e {edirection = Left} {point = (x, y)}

hushroomUpdate :: Float -> WorldGrid -> Enemy -> Enemy
hushroomUpdate eT g e@(Enemy (x,y) Hushroom s Left) | not (willEnemyHitWall e g) && enemyGrounded g e  = e {point = (x-0.5, y)}
                                                    | not $ enemyGrounded g e = e {point = (x-0.5, y)}
                                                    | otherwise = e {edirection = Right} {point = (x, y)}
hushroomUpdate eT g e@(Enemy (x,y) Hushroom s Right)| not (willEnemyHitWall e g) && enemyGrounded g e  = e {point = (x+0.5, y)}
                                                    | not $ enemyGrounded g e = e {point = (x+0.5, y)}
                                                    | otherwise = e {edirection = Left} {point = (x, y)}

dirCheck :: Enemy -> Hario -> Looking
dirCheck (Enemy (ex,_) _ _ _ ) (Hario (hx,_) _ _ _ _ _ _ _) | ex >= hx= Left
                                                            | ex < hx = Right

inProximity :: Float -> Enemy -> Hario -> Bool
inProximity d e@(Enemy (ex,ey) _ _ _) h@(Hario (hx,hy) _ _ _ _ _ _ _) | dirCheck e h == Left = ex-hx < d
                                                                      | otherwise = hx-ex <= d

enemyGroundedUpdate :: Float -> WorldGrid -> Enemy -> Enemy
enemyGroundedUpdate et g e | enemyGrounded g e &&  estate e /= EDie  = e
                           | otherwise = e{point = gravity(gravity (point e))}

enemyGrounded::WorldGrid -> Enemy -> Bool
enemyGrounded w e@(Enemy pos@(x,y) s p k) | let
    rightBorder = 16*fromIntegral (length (head w))
    downBorder  = -16*fromIntegral (length w)
    cordToInt cord = floor (cord/16)
    pointtofield (px,py) | px<0 || px>rightBorder || py<downBorder = W 0
                         | py>0 = A
                         | otherwise = (w!!cordToInt (-py))!!cordToInt px
    fieldSolid f = case f of
            A -> False
            C -> False
            X i -> False
            H -> False
            E _ -> False
            _ -> True
    size = getEnemySize e
    offx = fst size / 2
    offy = snd size / 2
    pointToCheck (px,py) = [(px,py-offy-8),(px-offx,py-offy-8),(px+offx,py-offy-8)]
    in any (fieldSolid . flip pointToField w) (pointToCheck (x,y)) = True
    | otherwise = False

anyNotDead :: (Enemy -> Bool) -> [Enemy] -> Bool
anyNotDead p e = any p (Prelude.filter (\x -> (estate x /= EDead) && (estate x /= EDie)) e)