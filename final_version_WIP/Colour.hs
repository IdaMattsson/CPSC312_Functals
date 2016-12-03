module Colour where

import Data.Word

-- Word8 is an 8-bit unsigned integer type
-- 0 is black, 255 is white
type Color = (Word8, Word8, Word8, Word8)

data Triple = AlphaTriple | FunctionTriple

type AlphaTriple = (Double, Double, Double)
type FunctionTriple = ((Double -> Double), (Double -> Double), (Double -> Double))

-- make an enum instead that takes a number input to choose the colour configuration? 


-- The x input to these functions will always be in the range 0 -> 1

-- one byte for the red color channel
colorRed :: Double -> Word8
colorRed x = truncate (x * 255)

-- one for green
colorGreen :: Double -> Word8
colorGreen x = truncate (x * 255)

-- and one for blue
colorBlue :: Double -> Word8
colorBlue x = truncate (x * 255)


funColorRed x alpha = truncate ((sin (x^4)) * alpha)
funColorGreen x alpha = truncate ((cos x) * alpha)
funColorBlue x alpha = truncate ((10*x) * alpha)



-- composite gradient function
color :: Double -> Color
color x = (colorRed x, colorGreen x, colorBlue x, 255)


-- Used for determining the alpha value of the colours
funColor:: Double -> AlphaTriple -> Color
funColor x aTr = (funColorRed x (get1 aTr), funColorGreen x (get2 aTr), funColorBlue x (get3 aTr), 255)


colorFunc:: Double -> FunctionTriple -> AlphaTriple -> Color
colorFunc x fTr aTr = (applyFunc x (get1 fTr) (get1 aTr), applyFunc x (get2 fTr) (get2 aTr), applyFunc x (get3 fTr) (get3 aTr), 255)

applyFunc :: Double -> (Double -> Double) -> Double -> Word8
applyFunc x f a = truncate ((f x) * a)


-- creates an array of colors representing a gradient of given width and height
-- output gradient is flattened
gradient :: Int -> Int -> [Color]
gradient w h
 = [(fromIntegral (div (x * 255) w), fromIntegral (div (y * 255) h), 0, 255)
    | x <- [0..w-1],
       y <- [0..h-1]]


get1 (a,b,c) = a
get2 (a,b,c) = b
get3 (a,b,c) = c


-- Some things to try:

{-
GIVES A BLUE BACKGROUND AND PURPLE INTERIOUR
funColourRed x = truncate ((sin (x^2)) * 150)
funColourGreen x = truncate ((cos x) * 40)
funColourBlue x = truncate ((15*x) * 255)-}