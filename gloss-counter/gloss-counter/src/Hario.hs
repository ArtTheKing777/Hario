module Hario where
import Model
import View (harioSpeed)
import Prelude hiding (Right, Left)
import Graphics.Gloss (Vector, Point)

updateHario :: Hario -> Hario
updateHario p@(Hario (x, y) s pow d v@(vx, vy) g l c) | g = Hario (x+vx, y+vy) s pow d v g l c
                                                      | otherwise = Hario (x+vx, y+vy) Fall pow d (gravity v) False l c

gravity :: Vector -> Vector
gravity (x,y) = (x,y-0.52)

moveLeft :: Hario -> Hario
moveLeft p@(Hario pos a pow dir (vx, vy) g l c)   | dir == Right = Hario pos Walk pow Left (-4, vy) g l c
                                            | otherwise    = Hario pos Walk pow dir (-4, vy) g l c

moveRight :: Hario -> Hario
moveRight p@(Hario pos a pow dir (vx, vy) g l c)  | dir == Left = Hario pos Walk pow Right (4, vy) g l c
                                            | otherwise   = Hario pos Walk pow dir (4, vy) g l c

idle :: Hario -> Hario
idle p@(Hario (x,y) s pow dir (vx, vy) g l c) = Hario (x,y) Idle pow dir (0, vy) g l c

getHarioSize :: Hario -> Point
getHarioSize h@(Hario pos s Small d v g l c) = (15,15)
getHarioSize h@(Hario pos s p d v g l c) = (15,33)



getHarioHitBoxCorners :: Hario -> [Point]
getHarioHitBoxCorners h@(Hario (x,y) s Small d v g l c) = [tl,dl,tr,dr]
    where size = getHarioSize h
          sizex = fst size
          sizey = snd size
          tl = (x-(sizex/2),y+(sizey/2))
          tr = (x+(sizex/2),y+(sizey/2))
          dl = (x-(sizex/2),y-(sizey/2))
          dr = (x+(sizex/2),y-(sizey/2))
getHarioHitBoxCorners h@(Hario (x,y) s _ d v g l c) = [tl,dl,ll,rl,al,tr,lr,rr,ar,dr]
    where size = getHarioSize h
          sizex = fst size
          sizey = snd size
          tl = (x-(sizex/2),y+(sizey/2))
          tr = (x+(sizex/2),y+(sizey/2))
          dl = (x-(sizex/2),y-(sizey/2))
          dr = (x+(sizex/2),y-(sizey/2))
          lr = (x+(sizex/2),y-(sizey/4))
          ll = (x-(sizex/2),y-(sizey/4))
          rr = (x+(sizex/2),y+(sizey/4))
          rl = (x-(sizex/2),y+(sizey/4))
          ar = (x-(sizex/2),y)
          al = (x+(sizex/2),y)

jump :: Hario -> Hario
jump p@(Hario (x,y) s pow dir (vx, vy) g l c) | not g      = p 
                                          | otherwise  = Hario (x,y) Jump pow dir (vx,8.5) False l c
