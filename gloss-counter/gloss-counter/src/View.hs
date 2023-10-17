-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view _  = getHario

getHario :: IO Picture
getHario = loadBMP "media/Super_Hario_Bros_Logo.bmp"



