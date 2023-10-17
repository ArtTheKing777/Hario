-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss ( loadBMP, Picture , pictures, scale, translate )
import Model ( GameState )


view :: GameState -> IO Picture
view _  = do
                hario <- getHario
                hario <- pure (scale 0.5 0.5 hario)
                hario <- pure (translate (-400) 0 hario)

                hario2 <- getHario
                hario2 <- pure (scale 0.5 0.5 hario2)
                hario2 <- pure (translate (400) 0 hario2)
                
                return (pictures [hario, hario2])




getHario :: IO Picture
getHario = loadBMP "media/Super_Hario_Bros_Logo.bmp"



