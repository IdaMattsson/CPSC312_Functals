-- CPSC 312 Project 2 --
-- Tim Straubinger and Ida Mattsson 
-- Link to wiki: http://wiki.ubc.ca/Course:CPSC312-2016-Project2-Functals


import Data.Word
import Data.ByteString
import Codec.BMP
import Mandelbrot
import Colour
 

-- turns a list of colors into a bytestring
-- for references: pack:: String -> ByteString
colorsToByteString :: [Color] -> ByteString
colorsToByteString c = Data.ByteString.concat [(pack [r, g, b, a]) | (r, g, b, a) <- c]

-- Saves a bitmap image of the given width and height to given file path filled with data from bytestring
-- bytestring must be the following format: [r,g,b,a,r,g,b,a,r,g,b,a...] where each r, g, b and a color
-- component is a byte
-- The size of the bytestring must be 4 * w * h bytes consequently
saveBMP :: Int -> Int -> [Char] -> ByteString -> IO ()
saveBMP w h path btstr
  = do
       let bmp = packRGBA32ToBMP w h btstr
       writeBMP path bmp


-- project screen coordinates to complex plane
-- (px, py) are screen cooradinates
-- (w, h) are the image dimensions
-- (cx, cy) are the image centers, changing these values shifts image centre
-- +x moves mandel to left (center to the right), +y moves mandel down (image upward)
project :: (Int, Int) -> (Int, Int) -> (Double, Double) -> Double -> Complex
project (px, py) (w, h) (cx, cy) magnification
  = ((((fromIntegral (px - (div w 2)) / fromIntegral (div w 2))) / magnification) + cx,
     (((fromIntegral (py - (div h 2)) / fromIntegral (div w 2))) / magnification) + cy)



-- this is it, guys -- 
-- saves you a fractal
-- Three different versions; 
--  renderMandelBrot genenrates a simple black and white fractal
--  cRenderMandelBrot generates a colored fractal, using a pre-defined color function
--  fcRenderMandelBrot generates a colored fractal, according to user-defined color assignment functions as well as alpha values
--  (all render functions will save to directory /images/filename, so make sure there is such a folder! (keeps things neater))


-- Basic Black and White MandelBrot --
-- usage:
-- renderMandelbrot (image_width, image_height) (center_real, center_imag) magnification filename
-- try: 
-- renderMandelbrot (250, 250) (0, 0) 0.3 "mandelbrot1.bmp"
-- renderMandelbrot (250, 250) (0.001643721971153, -0.822467633298876) 100 "mandelbrot2.bmp"
renderMandelbrot :: (Int, Int) -> (Double, Double) -> Double -> [Char] -> IO ()
renderMandelbrot (w, h) (x, y) mag filename
  = do
       let filePath = "images/" ++ filename
       let colors = [renderPoint x1 y1 | y1 <- [0..h-1], x1 <- [0..w-1]]
       let btstr = colorsToByteString colors
       saveBMP w h filePath btstr
    where
       renderPoint :: Int -> Int -> Color
       renderPoint x2 y2 = colorFunc (calculateMandelbrot (project (x2, y2) (w, h) (x, y) mag)) bw_triple whiteAlpha_Triple

-- Color Mandelbroth: allows user to input alpha colour scheme
-- uses random arbitrary colour assignment functions
-- usage:
-- cRenderMandelbrot (image_width, image_height) (center_real, center_imag) (alpha_red, alpha_green, alpha_blue) magnification filename
-- try:
-- cRenderMandelbrot (250, 250) (-1, 0) (150, 40, 255) 0.5 "mandel3.bmp"
-- cRenderMandelbrot (250, 250) (-1, 0) (150, 78, 800) 0.5 "mandel4.bmp"
cRenderMandelbrot :: (Int, Int) -> (Double, Double) -> AlphaTriple -> Double -> [Char] -> IO ()
cRenderMandelbrot (w, h) (x, y) aT mag filename
  = do
       let filePath = "images/" ++ filename
       let colors = [renderPoint x1 y1 | y1 <- [0..h-1], x1 <- [0..w-1]]
       let btstr = colorsToByteString colors
       saveBMP w h filePath btstr
    where
       renderPoint :: Int -> Int -> Color
       renderPoint x2 y2 = colorFunc (calculateMandelbrot (project (x2, y2) (w, h) (x, y) mag)) colorTriple aT


-- Color Function Mandelbrot: allows user to input both specific colour assignment functions and alpha colour scheme
-- functions can be inputed with simple operators, or using lambda notation
-- usage:
-- cfRenderMandelbrot (image_width, image_height) (center_real, center_imag) (colorFun_red, colorFun_green, colorFun_blue) (alpha_red, alpha_green, alpha_blue) magnification filename
-- try:
-- cfRenderMandelbrot (250, 250) (-1, 0) (sin, cos, (10*)) (150, 40, 255) 0.5 "mandel6.bmp"
-- cfRenderMandelbrot (250, 250) (-1, 0) (sin, (\x -> x^3), (10*)) (150, 40, 700) 0.5 "mandel6.bmp"
cfRenderMandelbrot :: (Int, Int) -> (Double, Double) -> FunctionTriple -> AlphaTriple -> Double -> [Char] -> IO ()
cfRenderMandelbrot (w, h) (x, y) fT aT mag filename
  = do
       let filePath = "images/" ++ filename
       let colors = [renderPoint x1 y1 | y1 <- [0..h-1], x1 <- [0..w-1]]
       let btstr = colorsToByteString colors
       saveBMP w h filePath btstr
    where
       renderPoint :: Int -> Int -> Color
       renderPoint x2 y2 = colorFunc (calculateMandelbrot (project (x2, y2) (w, h) (x, y) mag)) fT aT


-- and an additional test - just for fun! this is AWESOMESAUCE!!
-- cfRenderMandelbrot (250, 250) (-0.001643721971153, 0.8) ((\x -> sin x), (\x -> x^3), (10*)) (230, 167, 890) 100 "awesomesauceMandel.bmp"

