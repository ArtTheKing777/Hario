module Hario where
import Model
import View (harioSpeed)
import Prelude hiding (Right, Left)
import Graphics.Gloss (Vector, Point, Rectangle (Rectangle))
import GHC.Float (int2Float)

updateHario :: Hario -> Hario
updateHario p@(Hario (x, y) s pow d v@(vx, vy) g) | g = Hario (x+vx, y+vy) s pow d v g
                                                  | otherwise = Hario (x+vx, y+vy) Fall pow d (gravity v) False

enemyCollideCheck :: Hario -> Enemy -> Hario
enemyCollideCheck h e   | collidesWithEnemy e h && not (isFalling h) = case power h of
                                                                Small -> Hario (hpos h) Die (power h) (direction h) (velocity h) (onground h)
                                                                Big -> Hario (hpos h) (state h) Small (direction h) (velocity h) (onground h)
                                                                Fire -> Hario (hpos h) (state h) Small (direction h) (velocity h) (onground h)
                        | otherwise = h

gravity :: Vector -> Vector
gravity (x,y) = (x,y-0.52)

moveLeft :: Hario -> Hario
moveLeft p@(Hario pos a pow dir (vx, vy) g)   | dir == Right = Hario pos Walk pow Left (-5, vy) g
                                              | otherwise    = Hario pos Walk pow dir (-5, vy) g

moveRight :: Hario -> Hario
moveRight p@(Hario pos a pow dir (vx, vy) g)  | dir == Left = Hario pos Walk pow Right (5, vy) g
                                              | otherwise   = Hario pos Walk pow dir (5, vy) g

idle :: Hario -> Hario
idle p@(Hario (x,y) s pow dir (vx, vy) g) = Hario (x,y) Idle pow dir (0, vy) g

getHarioSize :: Hario -> Point
getHarioSize h@(Hario pos s Small d v g) = (15,15)
getHarioSize h@(Hario pos s p d v g)     = (15,33)

getHarioHitBoxCorners :: Hario -> [Point]
getHarioHitBoxCorners h@(Hario (x,y) s Small d v g) = [tl,dl,tr,dr]
    where size = getHarioSize h
          sizex = fst size
          sizey = snd size
          tl = (x-(sizex/2),y+(sizey/2))
          tr = (x+(sizex/2),y+(sizey/2))
          dl = (x-(sizex/2),y-(sizey/2))
          dr = (x+(sizex/2),y-(sizey/2))
getHarioHitBoxCorners h@(Hario (x,y) s _ d v g) = [tl,dl,ll,rl,al,tr,dr,lr,rr,ar]
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
jump p@(Hario (x,y) s pow dir (vx, vy) g) | not g      = p 
                                          | otherwise  = Hario (x,y) Jump pow dir (vx,8.5) False

isFalling :: Hario -> Bool
isFalling h | snd (velocity h) < 0 = True
            | otherwise            = False

collidesWithEnemy :: Enemy -> Hario -> Bool
collidesWithEnemy e h = intersects (point e,getEnemySize e) (hpos h, getHarioSize h)

intersects :: (Point, Point) -> (Point, Point) -> Bool
intersects r1 r2 = any (inbox r1) (corners r2) || any (inbox r2) (corners r1)

inbox :: (Point, Point) -> Point -> Bool
inbox ((x,y), (w,h)) (xp,yp) = xp >= x && xp <= x+w && yp >= y && yp <= y+h

corners :: (Point, Point) -> [Point]
corners ((x,y), (w,h)) = [(x,y), (x+w,y), (x,y+h), (x+w,y+h)]

