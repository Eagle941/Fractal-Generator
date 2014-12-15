--Matriculation number: s1443062
--Student name: Giuseppe Stratis Afentoulis

import Engine
import Codec.Picture (generateImage, writePng)
import Data.Word (Word8)
import Data.Complex (Complex(..), magnitude)

maxIter :: Int
maxIter = 500

plot :: RealFloat a => Complex a -> Complex a -> Int -> (Complex a, Int)
plot c z iter
  | iter >= maxIter = (1 :+ 1, 0)
  | magnitude z > 2 = (z', iter)
  | otherwise = plot c z' (iter + 1)
    where
		z' = z^2 + ((-0.8) :+ (0.156))

point :: Int -> Int -> Word8
point a b = draw . realize $ plot (x :+ y) (x :+ y) 0 
  where
    (x, y) = (coord xi xf width a, coord yi yf height b)
	
main :: IO ()
main = writePng "juliasetz2.png" $ generateImage point width height