module Data.Matrix.AsXYZ.Common (
  Sign(..),
  Var(..),
  rowVars,
  ) where

import Data.Ratio
import Numeric

data Sign a = P a | N a | Zero deriving (Show,Eq)
data Var a = X a | Y a | Z a | W a deriving (Show,Eq)

rowVars :: Integral a => [Ratio a] -> [Sign (Var (Ratio a))]
rowVars = reduceVars . sortVars . toVars
    
toVars :: Integral a => [Ratio a] -> [Sign (Var (Ratio a))]
toVars = zipWith (\a b -> hoge a b) [X,Y,Z,W]
      where
        hoge f r | r < 0 = N $ f (r * (-1))
                 | otherwise = P $ f r

sortVars :: Eq a => [Sign (Var a)] -> [Sign (Var a)]
sortVars parts | null hh = parts
               | otherwise = h: filter (/= h) parts
      where
        hh = filter isPrimary parts
        h = head hh

-- 正の係数がついた変数である
isPrimary :: Sign (Var a) -> Bool
isPrimary Zero = False
isPrimary (N _) = False
isPrimary (P (W _)) = False
isPrimary _ = True

reduceVars :: (Eq a, Num a) => [Sign (Var a)] -> [Sign (Var a)]
reduceVars rr = if null a then [Zero] else a
      where
        a = filter (not . isZero) rr

isZero :: (Eq a, Num a) => Sign (Var a) -> Bool
isZero (N v) = isZero' v
isZero (P v) = isZero' v
isZero Zero = True

isZero' :: (Eq a, Num a) => Var a -> Bool
isZero' (X n) = n == 0
isZero' (Y n) = n == 0
isZero' (Z n) = n == 0
isZero' (W n) = n == 0

