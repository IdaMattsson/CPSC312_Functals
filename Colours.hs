-- Simple Colour Library


assignChar :: Double -> Char 
assignChar mandelVal
 | mandelVal < 0.2 = ' '
 | mandelVal < 0.4 = '_'
 | mandelVal < 0.6 = '='
 | mandelVal < 0.8 = '@'
 | otherwise = '#'