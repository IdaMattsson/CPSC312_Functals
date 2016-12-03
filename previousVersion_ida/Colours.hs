-- Simple Colour Library

module Colours where



-- a very simple colour generator, with alpha 0
--assignColour :: Double -> [Double]

generateColourList mandelVals = 
 foldr (++) [] [assignColour(x) | x <- mandelVals]

-- colour assignment function
assignColour mandelVal = intoString [mandelVal, mandelVal, mandelVal, 0]

intoString doubleList = 
 foldr ((++) . show) [] doubleList


-- For Terminal Testing with Characters instead of colour assignment 
-- Simple Character assignment depending of the location of the point in the Mandelbrot set
assignChar :: Double -> Char 
assignChar mandelVal
 | mandelVal < 0.2 = ' '
 | mandelVal < 0.4 = '_'
 | mandelVal < 0.6 = '='
 | mandelVal < 0.8 = '@'
 | otherwise = '#'