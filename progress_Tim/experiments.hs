import Data.Word
import Data.ByteString
import Codec.BMP
import Mandelbrot

-- one byte for the red color channer
colorRed :: Double -> Word8
colorRed x = truncate (x * 255)

-- one for green
colorGreen :: Double -> Word8
colorGreen x = truncate (x * 255)

-- and one for blue
colorBlue :: Double -> Word8
colorBlue x = truncate (x * 255)

type Color = (Word8, Word8, Word8, Word8)

-- composite gradient function
color :: Double -> Color
color x = (colorRed x, colorGreen x, colorBlue x, 255)

-- turns a list of colors into a bytestring
colorsToByteString :: [Color] -> ByteString
colorsToByteString c = Data.ByteString.concat [(pack [r, g, b, a]) | (r, g, b, a) <- c]

-- creates an array of colors representing a gradient of given width and height
-- output gradient is flattened
gradient :: Int -> Int -> [Color]
gradient w h
 = [(fromIntegral (div (x * 255) w), fromIntegral (div (y * 255) h), 0, 255)
    | x <- [0..w-1],
       y <- [0..h-1]]

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
renderMandelbrot :: (Int, Int) -> (Double, Double) -> Double -> [Char] -> IO ()
renderMandelbrot (w, h) (x, y) mag path
  = do
       let colors = [renderPoint x1 y1 | y1 <- [0..h-1], x1 <- [0..w-1]]
       let btstr = colorsToByteString colors
       saveBMP w h path btstr
    where
       renderPoint :: Int -> Int -> Color
       renderPoint x2 y2 = color (calculateMandelbrot (project (x2, y2) (w, h) (x, y) mag))





