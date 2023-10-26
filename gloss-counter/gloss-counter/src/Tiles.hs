module Tiles where
import Fileload
import Graphics.Gloss.Data.Picture
import Graphics.Gloss (Rectangle(Rectangle), BitmapData (bitmapSize))

tiles :: IO [Picture]
tiles = do
            tilesbmp <- getTilesBmp
            return (makeListofSheet (Rectangle (0,-1) (16, -16)) (tilesbmp !! 0) (fst (bitmapSize (tilesbmp !! 0))) ++ makeListofSheet (Rectangle (0,-1) (16, -16)) (tilesbmp !! 1) (fst (bitmapSize (tilesbmp !! 1))))

pipe :: IO [Picture]
pipe = do
        pipebmp <- getPipeBmp
        return (makeListofSheet (Rectangle (0, -1) (32, -32)) pipebmp (fst (bitmapSize pipebmp)))

coins :: IO [Picture]
coins = do
            coinsbmp <- getCoinsBmp
            return (makeListofSheet (Rectangle (0, -1) (16, -16)) coinsbmp (fst (bitmapSize coinsbmp)))

flag :: IO [Picture]
flag = do
        flapbmp <- getFlagBmp
        return (makeListofSheet (Rectangle (0, -1) (16, -16)) flapbmp (fst (bitmapSize flapbmp)))

hoolitbilltower :: IO Picture
hoolitbilltower = do
                    bitmap <$> getHoolitBillTowerBmp