module Fileload where
import Graphics.Gloss.Data.Bitmap (BitmapData(bitmapSize), Rectangle (Rectangle), loadBMP)
import Graphics.Gloss.Data.Picture (Picture (Bitmap, BitmapSection, Blank) , pictures, scale, translate, bitmapSection, bitmap)
 -- Load from filepath as BitmapData
loadBitmapData :: FilePath -> IO BitmapData
loadBitmapData s = do
                        p@(Bitmap bmpdata) <- loadBMP s
                        return bmpdata

-- load individual things
getHario :: IO BitmapData
getHario = loadBitmapData "media/Super_Hario_Bros_Logo.bmp"

getHarioAnimationSheet :: IO BitmapData
getHarioAnimationSheet = loadBitmapData "media/hario.bmp"

getFireHarioAnimationSheet :: IO BitmapData
getFireHarioAnimationSheet = loadBitmapData "media/firehario.bmp"

getSmallHarioAnimationSheet :: IO BitmapData
getSmallHarioAnimationSheet = loadBitmapData "media/smallhario.bmp"

getTextBox :: IO BitmapData
getTextBox = loadBitmapData "media/textBox.bmp"
