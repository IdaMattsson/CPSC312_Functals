
import Data.Complex
import Data.Colour

edge = 128

side n v0 v1 =
 let sv = (v1 - v0) / fromIntegral n
 in  [v0, (v0 + sv) .. v1]

sideX = side edge (-2) 2
sideY = side edge (-2) 2

grid = map (\y -> map (:+ y) sideX) sideY