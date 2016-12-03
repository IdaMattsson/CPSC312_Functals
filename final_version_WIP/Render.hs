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


-- this is it, guys
-- saves you a fractal
-- usage:
-- renderMandelbrot (image_width, image_height) (center_real, center_imag) magnification filename
-- try: 
-- renderMandelbrot (250, 250) (0, 0) 0.3 "mandelbrot1.bmp"
-- renderMandelbrot (250, 250) (0.001643721971153, -0.822467633298876) 100 "mandelbrot2.bmp"

{- TRYING TO make modular, but not helping a lot... 

TODO COULD POTENTIALLY USE MAYBE OR EITHER

renderSetUp_Basic:: (Int, Int) -> [Char] -> ByteString
renderSetUp_Basic (w,h) (x, y) mag = 
 colorsToByteString [renderPoint x1 y1 (w, h)| y1 <- [0..h-1], x1 <- [0..w-1]] 

-- renderSetUp_Alpha (w,h

renderPoint :: Int -> Int -> (Int, Int) -> (Double, Double) -> Double -> Color
renderPoint x2 y2 (w, h) (x, y) mag = color (calculateMandelbrot (project (x2, y2) (w, h) (x, y) mag))

renderAPoint :: Int -> Int -> (Int, Int) -> (Double, Double) -> Double -> AlphaTriple -> Color
renderAPoint x2 y2 (w, h) (x, y) mag aT = funColor (calculateMandelbrot (project (x2, y2) (w, h) (x, y) mag)) aT

renderFAPoint :: Int -> Int -> (Int, Int) -> (Double, Double) -> Double -> FunctionTriple -> AlphaTriple -> Color
renderFAPoint x2 y2 (w, h) (x, y) mag fT aT = colorFunc (calculateMandelbrot (project (x2, y2) (w, h) (x, y) mag)) fT aT

-}


-- this is it, guys
-- saves you a fractal
-- Three different versions; 
--  renderMandelBrot genenrates a simple black and white fractal
--  cRenderMandelBrot generates a colored fractal, using a pre-defined color function
--  fcRenderMandelBrot generates a colored fractal, according to user-defined color assignment functions as well as alpha values


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


fcRenderMandelbrot :: (Int, Int) -> (Double, Double) -> FunctionTriple -> AlphaTriple -> Double -> [Char] -> IO ()
fcRenderMandelbrot (w, h) (x, y) fT aT mag filename
  = do
       let filePath = "images/" ++ filename
       let colors = [renderPoint x1 y1 | y1 <- [0..h-1], x1 <- [0..w-1]]
       let btstr = colorsToByteString colors
       saveBMP w h filePath btstr
    where
       renderPoint :: Int -> Int -> Color
       renderPoint x2 y2 = colorFunc (calculateMandelbrot (project (x2, y2) (w, h) (x, y) mag)) fT aT

