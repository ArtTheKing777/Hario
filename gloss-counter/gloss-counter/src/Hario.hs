module Hario where
import Model
import View (harioSpeed)
import Prelude hiding (Right, Left)
import Graphics.Gloss (Vector)

updateHario :: Hario -> Hario
updateHario p@(Hario (x, y) s pow d v@(vx, vy) g) | g = Hario (x+vx, y+vy) s pow d v g
                                                  | y <= 0 = Hario (x+vx, y+vy) s pow d (vx, 0) True
                                                  | otherwise = Hario (x+vx, y+vy) s pow d (gravity v) False

gravity :: Vector -> Vector
gravity (x,y) = (x,y-1)

moveLeft :: Hario -> Hario
moveLeft p@(Hario pos a pow dir (vx, vy) g)   | dir == Right = Hario pos Walk pow Left (-5, vy) g
                                            | otherwise    = Hario pos Walk pow dir (-5, vy) g

moveRight :: Hario -> Hario
moveRight p@(Hario pos a pow dir (vx, vy) g)  | dir == Left = Hario pos Walk pow Right (5, vy) g
                                            | otherwise   = Hario pos Walk pow dir (5, vy) g

idle :: Hario -> Hario
idle p@(Hario (x,y) s pow dir (vx, vy) g) = Hario (x,y) Idle pow dir (0, vy) g

jump :: Hario -> Hario
jump p@(Hario (x,y) s pow dir (vx, vy) g) | y > 0     = p 
                                          | otherwise = Hario (x,y) Jump pow dir (vx, 10) False
