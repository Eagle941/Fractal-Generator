--Matriculation number: s1443062
--Student name: Giuseppe Stratis Afentoulis

import Codec.Picture (generateImage, writePng)
import Data.Word (Word8)
import Data.Complex (Complex(..), magnitude)


maxIter :: Int
maxIter = 40

plot :: RealFloat a => Complex a -> Complex a -> Int -> (Complex a, Int)
plot c z iter
  | iter >= maxIter = (1 :+ 1, 0)
  | magnitude z > 2 = (z', iter)
  | otherwise = plot c z' (iter + 1)
	where
		z' = z^2 + ((1 - phi) :+ 0)
			where
				phi = ( 1 + (sqrt 5)) / 2

point :: Int -> Int -> Word8
point a b = draw . realize $ plot (x :+ y) (x :+ y) 0 
  where
    (x, y) = (coord xi xf width a, coord yi yf height b)
	
main :: IO ()
main = writePng "juliasetphi.png" $ generateImage point width height

xi, yi, xf, yf :: Double
(xi, yi) = (-2.0000, -2.0000)
(xf, yf) = (2.0000, 2.0000)

width, height :: Int
(width, height) = (f, f)
	where
		f = 4000

realize :: RealFloat a => (Complex a, Int) -> a
realize (z, iter) = (fromIntegral iter - log (log (magnitude z))) / fromIntegral (maxIter)

coord :: Double -> Double -> Int -> Int -> Double
coord n0 n1 a ni = (n1 - n0) * fromIntegral ni / fromIntegral a + n0

draw f = truncate . (* 255) . cut $ 1 - f
	where
		cut v = 1 - exp (-exp ((v - 0.90) / 0.030))