--Matriculation number: s1443062
--Student name: Giuseppe Stratis Afentoulis

module Engine where

import Codec.Picture (generateImage, writePng)
import Data.Word (Word8)
import Data.Complex (Complex(..), magnitude)

xi, yi, xf, yf :: Double -- This function defines the dimentions of the canvas
(xi, yi) = (-2.0000, -2.0000)
(xf, yf) = (2.0000, 2.0000)

width, height :: Int -- This function defines the resolution of the graph
(width, height) = (f, f)
	where
		f = 2000

realize :: RealFloat a => (Complex a, Int) -> a
realize (z, iter) = (fromIntegral iter - log (log (magnitude z))) / fromIntegral 1000

coord :: Double -> Double -> Int -> Int -> Double
coord n0 n1 a ni = (n1 - n0) * fromIntegral ni / fromIntegral a + n0

draw f = truncate . (* 255) . cut $ 1 - f
	where
		cut v = 1 - exp (-exp ((v - 0.90) / 0.030))