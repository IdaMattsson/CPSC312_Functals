-- Fractal Functions

module Functals where

import Complex

 
-- max number of iterations
max_it = 100

-- escape limit
escp_lim = 1000

-- Computes the number of iterations before escaping the Mandelbrot set
-- begins iterations of mandelbrot set with z = 0 and c as a given point position
computePoint :: (Double, Double) -> Double
computePoint pos = helper_computePoint (0, 0) pos 0

-- helper_computePoint for computing the number of iterations within the set
-- compute_point represents one iteration of the Mandelbrot set 
-- given the complex numbers c, and z, either returns or iterates
-- with new z as z0^2 + c, where z0 is previous value for z
-- returns if z exceeds the maximum number of iterations, or absolute value of z escaped the set boundary
helper_computePoint :: (Double, Double) -> (Double, Double) -> Int -> Double
helper_computePoint z c it
 | c_abs z >= escp_lim = (fromIntegral(it) / fromIntegral(max_it))
 | it == max_it = 1
 | otherwise = helper_computePoint (add (square z) c) c (it + 1)




{-
-- Simple Character assignment depending of the location of the point in the Mandelbrot set
assignChar :: Double -> Char 
assignChar mandelVal
 | mandelVal < 0.2 = ' '
 | mandelVal < 0.4 = '_'
 | mandelVal < 0.6 = '='
 | mandelVal < 0.8 = '@'
 | otherwise = '#'

-}