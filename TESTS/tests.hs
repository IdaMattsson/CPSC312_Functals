-- checkPalindrome :: IO String
--import System.IO
import Data.ByteString
import qualified Data.ByteString.Char8 as C
import Codec.BMP

--writeTest :: String -> FilePath -> IO ()
writeTest string location = 
 do
  byteString <- return (C.pack string)
  Data.ByteString.writeFile location byteString

bmpTest location =
  do
    let rgba = C.pack "sjrku3njd9r3ifhhsjrku3njd0rn3ifhsrrku3nj9rn3ifhsj2rku3nd9rn3hifh"
    let bmp = packRGBA32ToBMP 4 4 rgba
    writeBMP location bmp