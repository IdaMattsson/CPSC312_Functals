module Mandelbrot where

type Complex = (Double, Double)

c_sqr :: Complex -> Complex
c_sqr (r, i) = ((r * r) - (i * i), (2 * r * i))

c_add :: Complex -> Complex -> Complex
c_add (r1, i1) (r2, i2) = (r1 + r2, i1 + i2)

c_abs :: Complex -> Double
c_abs (r, i) = sqrt ((r * r) + (i * i))

max_iterations :: Int
max_iterations = 100

escape_limit :: Double
escape_limit = 1000

calculateMandelbrot :: Complex -> Double
calculateMandelbrot c = helper (0, 0) c 0
  where 
    helper :: Complex -> Complex -> Int -> Double
    helper z c it
          | c_abs z > escape_limit = (fromIntegral it / fromIntegral max_iterations)
          | it == max_iterations = 1
          | otherwise = helper (c_add (c_sqr z) c) c (it + 1)