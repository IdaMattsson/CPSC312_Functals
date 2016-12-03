module Colour where

import Data.Word

-- Word8 is an 8-bit unsigned integer type
-- 0 is black, 255 is white
type Color = (Word8, Word8, Word8, Word8)

data Triple = AlphaTriple | FunctionTriple

type AlphaTriple = (Double, Double, Double)
type FunctionTriple = ((Double -> Double), (Double -> Double), (Double -> Double))

-- The x input to these functions will always be in the range 0 -> 1

-- assigns colour to the given point, given function and alpha triples 
colorFunc:: Double -> FunctionTriple -> AlphaTriple -> Color
colorFunc x fTr aTr = (applyFA x (get1 fTr) (get1 aTr), applyFA x (get2 fTr) (get2 aTr), applyFA x (get3 fTr) (get3 aTr), 255)

-- applies a colour assignment function to a argument, and multiplies result with alpha value
applyFA :: Double -> (Double -> Double) -> Double -> Word8
applyFA x f a = truncate ((f x) * a)


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

-- some predefined functions --

-- when no alpha is given
whiteAlpha_Triple :: AlphaTriple
whiteAlpha_Triple = (255, 255, 255)

-- black and white function
bw_fun:: (Double -> Double)
bw_fun = (\x -> (x * 1))

-- black and white function triple
bw_triple:: FunctionTriple
bw_triple = (bw_fun, bw_fun, bw_fun)

-- random predefined color assignment function
red_fun:: (Double -> Double)
red_fun = (\x -> sin (x^4))
green_fun:: (Double -> Double)
green_fun = (\x -> cos x)
blue_fun:: (Double -> Double)
blue_fun = (\x -> (10 * x))

colorTriple :: FunctionTriple
colorTriple = (red_fun, green_fun, blue_fun)




-- Some things to try:

{-
GIVES A BLUE BACKGROUND AND PURPLE INTERIOUR
funColourRed x = truncate ((sin (x^2)) * 150)
funColourGreen x = truncate ((cos x) * 40)
funColourBlue x = truncate ((15*x) * 255)-}