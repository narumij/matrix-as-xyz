module Data.Matrix.ReadTriplet (
  ReadTriplet(..),
  matrix4x4From3x4,
) where

import Control.Arrow

import Data.Ratio
import Data.Matrix
import Data.Matrix.CoordinateTriplet

matrix4x4From3x4 :: Num a => Matrix a -> Matrix a
matrix4x4From3x4 m = m <-> fromList 1 4 [0,0,0,1]

read3x4S' :: Fractional a => Int -> ReadS (Matrix a)
read3x4S' n s = first (fmap realToFrac) <$> read3x4S n s

read3x4S'' :: Integral a => Int -> ReadS (Matrix a)
read3x4S'' n s = first (fmap (floor . realToFrac)) <$> read3x4S n s

------------------

class ReadTriplet a where
  readTriplet :: Int -> ReadS (Matrix a)

instance (ReadTriplet a) => Read (Matrix a) where
   readsPrec = readTriplet

------------------

instance (Integral a) => ReadTriplet (Ratio a) where
  readTriplet = read3x4S

instance ReadTriplet Double where
  readTriplet = read3x4S'

instance ReadTriplet Float where
  readTriplet = read3x4S'

instance ReadTriplet Integer where
  readTriplet = read3x4S''

instance ReadTriplet Int where
  readTriplet = read3x4S''

----------------------
