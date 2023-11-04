module Fileload where
import Graphics.Gloss.Data.Bitmap (BitmapData(bitmapSize), Rectangle (Rectangle), loadBMP)
import Graphics.Gloss.Data.Picture (Picture (Bitmap, BitmapSection, Blank) , pictures, scale, translate, bitmapSection, bitmap)
 -- Load from filepath as BitmapData
loadBitmapData :: FilePath -> IO BitmapData
loadBitmapData s = do
                        p@(Bitmap bmpdata) <- loadBMP s
                        return bmpdata

-- rectangle for first sprite, bitmapdata of image
makeListofSheet :: Rectangle -> BitmapData -> Int -> [Picture]
makeListofSheet r@(Rectangle (x,y) (w,h)) bmp l | l >= w  = BitmapSection r bmp : makeListofSheet (Rectangle (x+w+1, y) (w,h)) bmp (l-w)
                                                | otherwise = []

--need them swapped somewhere
makeListofSheet2 :: Rectangle -> Int -> BitmapData ->  [Picture]
makeListofSheet2 r@(Rectangle (x,y) (w,h)) l  bmp |  l >= w  = BitmapSection r bmp : makeListofSheet2 (Rectangle (x+w, y) (w,h)) (l-w) bmp
                                                  | otherwise = []
                        

-- load individual things
getHarioBmp :: IO BitmapData
getHarioBmp = loadBitmapData "media/Super_Hario_Bros_Logo.bmp"

getHarioAnimationSheetBmp :: IO BitmapData
getHarioAnimationSheetBmp = loadBitmapData "media/hario/hario.bmp"

getFireHarioAnimationSheetBmp :: IO BitmapData
getFireHarioAnimationSheetBmp = loadBitmapData "media/hario/firehario.bmp"

getSmallHarioAnimationSheetBmp :: IO BitmapData
getSmallHarioAnimationSheetBmp = loadBitmapData "media/hario/smallhario.bmp"

getTextBoxBmp :: IO BitmapData
getTextBoxBmp = loadBitmapData "media/textBox.bmp"

getHenemiesBmp :: IO BitmapData
getHenemiesBmp = loadBitmapData "media/enemies/henemies.bmp"

getHowserBmp :: IO BitmapData
getHowserBmp = loadBitmapData "media/enemies/howser.bmp"

getHammerBmp :: IO BitmapData
getHammerBmp = loadBitmapData "media/enemies/hammer.bmp"

getFireBmp :: IO BitmapData
getFireBmp = loadBitmapData "media/enemies/fire.bmp"

getFireBallBmp :: IO BitmapData
getFireBallBmp = loadBitmapData "media/enemies/fireball.bmp"

getWormBmp :: IO [BitmapData]
getWormBmp = do
                move <- loadBitmapData "media/enemies/Worm/Maggot-Move.bmp" 
                spit <- loadBitmapData "media/enemies/Worm/Maggot-Spit.bmp"
                charge <- loadBitmapData "media/enemies/Worm/Maggot-Charge.bmp"
                return [move, spit, charge]

getAcidBmp :: IO [BitmapData]
getAcidBmp = do
            acidmove <- loadBitmapData "media/enemies/Worm/AcidBlob-Move.bmp"
            acidsplat <- loadBitmapData "media/enemies/Worm/AcidSplat.bmp"
            return [acidmove, acidsplat]

--load tiles

getTilesBmp :: IO [BitmapData]
getTilesBmp = do
            browntiles1 <- loadBitmapData "media/tiles/browntiles1.bmp"
            browntiles2 <- loadBitmapData "media/tiles/browntiles2.bmp"
            return [browntiles1, browntiles2]
        
getPipeBmp :: IO BitmapData
getPipeBmp = loadBitmapData "media/tiles/pipe.bmp"

getCoinsBmp :: IO BitmapData
getCoinsBmp = loadBitmapData "media/tiles/coins.bmp"

getFlagBmp :: IO BitmapData
getFlagBmp = loadBitmapData "media/tiles/flag.bmp"

getHoolitBillTowerBmp :: IO BitmapData
getHoolitBillTowerBmp = loadBitmapData "media/tiles/bulletbilltower.bmp"

getLevel :: String -> IO [[Char]]
getLevel s = do
    l <- readFile ("media/levels/level"++s++".txt")
    return (lines l)
            