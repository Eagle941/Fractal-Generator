--Matriculation number: s1443062
--Student name: Giuseppe Stratis Afentoulis

import Engine
import Codec.Picture (generateImage, writePng)
import Data.Word (Word8)
import Data.Complex (Complex(..), magnitude)

maxIter :: Int -- Total number of iterations
maxIter = 3333

plot :: RealFloat a => Complex a -> Complex a -> Int -> (Complex a, Int) 
plot c z iter
  | iter >= maxIter = (1 :+ 1, 0)
  | magnitude z > 2 = (z', iter)
  | otherwise = plot c z' (iter + 1)
    where
		z' = exp(z^3) - (0.59 :+ 0)

point :: Int -> Int -> Word8 -- Determines the points of the fractal
point a b = draw . realize $ plot (x :+ y) (x :+ y) 0 
  where
    (x, y) = (coord xi xf width a, coord yi yf height b)
	
main :: IO () -- The function to output the graph
main = writePng "juliasetz3.png" $ generateImage point width height