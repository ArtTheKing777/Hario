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

-- load individual things
getHarioBmp :: IO BitmapData
getHarioBmp = loadBitmapData "media/Super_Hario_Bros_Logo.bmp"

getHarioAnimationSheetBmp :: IO BitmapData
getHarioAnimationSheetBmp = loadBitmapData "media/hario.bmp"

getFireHarioAnimationSheetBmp :: IO BitmapData
getFireHarioAnimationSheetBmp = loadBitmapData "media/firehario.bmp"

getSmallHarioAnimationSheetBmp :: IO BitmapData
getSmallHarioAnimationSheetBmp = loadBitmapData "media/smallhario.bmp"

getTextBoxBmp :: IO BitmapData
getTextBoxBmp = loadBitmapData "media/textBox.bmp"

getHenemiesBmp :: IO BitmapData
getHenemiesBmp = loadBitmapData "media/henemies.bmp"

getHowserBmp :: IO BitmapData
getHowserBmp = loadBitmapData "media/howser.bmp"

getHammerBmp :: IO BitmapData
getHammerBmp = loadBitmapData "media/hammer.bmp"

getFireBmp :: IO BitmapData
getFireBmp = loadBitmapData "media/fire.bmp"

getWormBmp :: IO [BitmapData]
getWormBmp = do
                move <- loadBitmapData "media/worm/Maggot-Move.bmp" 
                spit <- loadBitmapData "media/worm/Maggot-Spit.bmp"
                charge <- loadBitmapData "media/worm/Maggot-Charge.bmp"
                return [move, spit, charge]

getAcidBmp :: IO [BitmapData]
getAcidBmp = do
            acidmove <- loadBitmapData "media/worm/AcidBlob-Move.bmp"
            acidsplat <- loadBitmapData "media/worm/AcitSplat.bmp"
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
    l <- readFile ("media/Levels/Level"++s)
    return (lines l)
            