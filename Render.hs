-- Program for actual render functions
module Render where
-- we do a bunch of fun stuffs

import Functals
import Colours

import Data.ByteString
import qualified Data.ByteString.Char8 as C
import Codec.BMP




-- assume square picture...?
-- input is the Mandelbrot points


generateBMP location w h = 
 do
  -- do something to get the width and the height depending on the (width, height) <- the generated string is always of given (width+1) * (height+1)
  -- assign colour to each point on the thing
  let rgba = C.pack (renderColouredMandel (w-1) (h-1)) 
  let bmp = packRGBA32ToBMP w h rgba
  writeBMP location bmp

-- AM I PERHAPS REALY CONFUSED BY HOW THIS WORKS???
-- pack is for strings
-- test function for BMP generation
bmpTest location =
  do
    let rgba = C.pack "sjrku3njd9r3ifhhsjrku3njd0rn3ifhsrrku3nj9rn3ifhsj2rku3nd9rn3hifh"
    let bmp = packRGBA32ToBMP 4 4 rgba
    writeBMP location bmp


renderColouredMandel w h =
 generateColourList [computePoint (x,y) | (x, y) <- generateGrid w h]
 -- generate a grid
 -- pass each element of the grid to compute point in mandel set
 -- pass list of mandel values to generateColourList
 -- what returns is the coloured mandel string of colour numbers

-- renders Mandelbrot for testing - remember to scale window!
-- 55 * 180 good for full size Terminal window
renderCharMandel width height =
 [assignChar(computePoint x) | x <- (generateGrid width height)]


-- Used to generate a point grid based on some input height and width parameters
generateGrid w h = 
 [((fromIntegral x - (fromIntegral w / 2)) / fromIntegral w * 2,
 (fromIntegral y - (fromIntegral h / 2)) / fromIntegral h * 4) | x <- [0..w], y <- [0..h]]







