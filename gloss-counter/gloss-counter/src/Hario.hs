module Hario where
import Model
import View (harioSpeed)
import Prelude hiding (Right, Left)
import Graphics.Gloss (Vector, Point, Rectangle (Rectangle))
import GHC.Float (int2Float)

updateHario :: Hario -> Hario
updateHario p@(Hario (x, y) s pow d v@(vx, vy) g l c) | g = Hario (x+vx, y+vy) s pow d v g l c
                                                      | otherwise = Hario (x+vx, y+vy) Fall pow d (gravity v) False l c

enemyCollideCheck :: [Enemy] -> Hario -> Hario
enemyCollideCheck e h   | anyNotDead (collidesWithEnemy h) e && isFalling h = case findEnemyThatCollideWithHario e h of
                                                                                Just Hushroom -> getPowerUp Hushroom h
                                                                                Just HireFlower -> getPowerUp HireFlower h
                                                                                _ ->  jump (h {onground = True})
                        | anyNotDead (collidesWithEnemy h) e && not (isFalling h) = 
                                                            case findEnemyThatCollideWithHario e h of
                                                                Just Hushroom -> getPowerUp Hushroom h
                                                                Just HireFlower -> getPowerUp HireFlower h
                                                                _        -> case power h of
                                                                                Small -> Hario (hpos h) Die (power h) (direction h) (velocity h) (onground h) (lives h) (coins h)
                                                                                Big -> Hario (hpos h) (state h) Small (direction h) (velocity h) (onground h) (lives h) (coins h)
                                                                                Fire -> Hario (hpos h) (state h) Big (direction h) (velocity h) (onground h) (lives h) (coins h)
                        | otherwise = h

findEnemyThatCollideWithHario :: [Enemy] -> Hario -> Maybe EnemyType
findEnemyThatCollideWithHario [] h = Nothing
findEnemyThatCollideWithHario (x:xs) h | collidesWithEnemy h x = Just (etype x)
                                       | otherwise = findEnemyThatCollideWithHario xs h

getPowerUp :: EnemyType -> Hario -> Hario
getPowerUp Hushroom h = case power h of
                        Fire -> h
                        Big -> h
                        Small -> Hario (fst(hpos h),snd(hpos h)+16) (state h) Big (direction h) (velocity h) (onground h) (lives h) (coins h)
getPowerUp HireFlower h = case power h of
                        Fire -> h
                        Big ->  Hario (hpos h) (state h) Fire (direction h) (velocity h) (onground h) (lives h) (coins h)
                        Small -> Hario (fst(hpos h),snd(hpos h)+16) (state h) Fire (direction h) (velocity h) (onground h) (lives h) (coins h)


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
jump p@(Hario (x,y) s pow dir (vx, vy) g _ _) | not g      = p 
                                              | otherwise  = Hario (x,y) Jump pow dir (vx,8.5) False (lives p) (coins p)

isFalling :: Hario -> Bool
isFalling h | snd (velocity h) < 0 = True
            | otherwise            = False

collidesWithEnemy :: Hario -> Enemy -> Bool
collidesWithEnemy h@(Hario (hx,hy) _ _ _ _ _ _ _) e@(Enemy(ex,ey) _ _ _) = intersects ((ex-(fst(getEnemySize e)/2),ey+(snd(getEnemySize e)/2)),getEnemySize e) ((hx-(fst(getHarioSize h)/2),hy+(snd(getHarioSize h)/2)), getHarioSize h)

enemyStompedCheck :: Hario -> [Enemy] -> [Enemy]
enemyStompedCheck _ []                      = []
enemyStompedCheck h (e@(Enemy p t s d):es)  | collidesWithEnemy h e && isFalling h = Enemy p t EDie d : enemyStompedCheck h es
                                            | otherwise = e : enemyStompedCheck h es
enemyStompedCheck h e@(Enemy p Hushroom s d) 
                                      | collidesWithEnemy h e = Enemy p Hushroom EDie d
                                      | otherwise = e
enemyStompedCheck h e@(Enemy p HireFlower s d) 
                                      | collidesWithEnemy h e = Enemy p HireFlower EDie d
                                      | otherwise = e
enemyStompedCheck h e@(Enemy p t s d) | collidesWithEnemy h e && isFalling h = Enemy p t EDie d
                                      | otherwise = e

intersects :: (Point, Point) -> (Point, Point) -> Bool
intersects r1 r2 = any (inbox r1) (corners r2) || any (inbox r2) (corners r1)

inbox :: (Point, Point) -> Point -> Bool
inbox ((x,y), (w,h)) (xp,yp) = xp >= x && xp <= x+w && yp >= y && yp <= y+h

corners :: (Point, Point) -> [Point]
corners ((x,y), (w,h)) = [(x,y), (x+w,y), (x,y+h), (x+w,y+h)]
