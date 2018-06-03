{- |
Module      : Data.Matrix.AsXYZ
Copyright   : (c) Jun Narumi 2017-2018
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

general equivalnet positionと4x4行列の相互変換をする

-}
module Data.Matrix.AsXYZ (
  fromXYZ,
  fromXYZ',
  fromABC,
  prettyXYZ,
  prettyABC,
  ) where

import Numeric
import Data.Char
import Data.List
import Data.Ratio
import Data.Matrix (Matrix,fromList,fromLists,toLists,identity,zero,(<->))
import Text.ParserCombinators.Parsec

import Data.Ratio.Slash
import Data.Matrix.AsXYZ.Parse

-- | Create a matirx from xyz coordinate string of general equivalent position
--
-- >                                      ( 1 % 1 0 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 1 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 0 % 1 1 % 1 0 % 1 )
-- > fromXYZ "x,y,z" :: Matrix Rational = ( 0 % 1 0 % 1 0 % 1 1 % 1 )
-- >
-- >                                                  ( 1 % 1 0 % 1 0 % 1 1 % 2 )
-- >                                                  ( 0 % 1 1 % 1 0 % 1 1 % 3 )
-- >                                                  ( 0 % 1 0 % 1 1 % 1 1 % 4 )
-- > fromXYZ "x+1/2,y+1/3,z+1/4" :: Matrix Rational = ( 0 % 1 0 % 1 0 % 1 1 % 1 )
-- >
-- >                                                              (  1  2  3  4 )
-- >                                                              (  5  6  7  8 )
-- >                                                              (  9 10 11 12 )
-- > fromXYZ "x+2y+3z+4,5x+6y+7z+8,9x+10y+11z+12" :: Matrix Int = (  0  0  0  1 )
fromXYZ :: Integral a => String -> Matrix (Ratio a)
fromXYZ input = unsafeGet $ makeMatrix <$> parse (equivalentPositions ratio) input input

-- | Maybe version
fromXYZ' :: Integral a => String -> Maybe (Matrix (Ratio a))
fromXYZ' input = get $ makeMatrix <$> parse (equivalentPositions ratio) input input

-- | uses abc instead of xyz
fromABC :: Integral a => String -> Matrix (Ratio a)
fromABC input = unsafeGet $ makeMatrix <$> parse (transformPpABC ratio) input input

makeMatrix :: Num a => [[a]] -> Matrix a
makeMatrix m = fromLists m <-> fromLists [[0,0,0,1]]

unsafeGet :: Either ParseError a -> a
unsafeGet e = case e of
  Left s -> error $ show s
  Right m -> m

get :: Either ParseError a -> Maybe a
get e = case e of
  Left s -> Nothing
  Right m -> Just m

----------------------------------

signedRatioString :: (Integral a) => Ratio a -> String
signedRatioString n | head s == '-' = s
            | otherwise = "+" ++ s
  where s = show $ Slash n

varString :: (Integral a) => Ratio a -> String -> String
varString num label | num == 0 = ""
                    | null label = signedRatioString num
                    | num == 1 = "+" ++ label
                    | num == -1 = "-" ++ label
                    | otherwise = signedRatioString num ++ label

rowString :: (Integral a) => [String] -> [Ratio a] -> String
rowString st line | null s = "0"
                  | head s == '+' = tail s
                  | otherwise = s
  where
     vars = zipWith varString line st
     s = foldl1 (++) $ varSort vars

varSort :: [String] -> [String]
varSort parts = case find (\x->(not . null) x && hasLetter x && isPositive x) parts of
  Just x -> x : filter (/=x) parts
  Nothing -> parts
  where
    hasLetter = isAlpha . last
    isPositive str = head str == '+'

xyzLabel :: [String]
xyzLabel = ["x","y","z",""]

abcLabel :: [String]
abcLabel = ["a","b","c",""]

showAs :: (Integral a) => [String] -> Matrix (Ratio a) -> String
showAs labels m = foldl1 (\x y-> x++","++y) $ map (rowString labels) $ take 3 $ toLists m

showAsXYZ :: (Integral a) => Matrix (Ratio a) -> String
showAsXYZ = showAs xyzLabel

showAsABC :: (Integral a) => Matrix (Ratio a) -> String
showAsABC = showAs abcLabel

-- | Get the xyz coordinate string of matrix
--
-- >>> prettyXYZ (identity 4 :: Matrix Int)
-- "x,y,z"
--
-- >           ( 0 % 1 0 % 1 0 % 1 1 % 2 )
-- >           ( 0 % 1 0 % 1 0 % 1 2 % 3 )
-- >           ( 0 % 1 0 % 1 0 % 1 4 % 5 )
-- > prettyXYZ ( 0 % 1 0 % 1 0 % 1 1 % 1 ) = "1/2,2/3,4/5"
prettyXYZ :: (Integral a) => Matrix (Ratio a) -> String
prettyXYZ = showAsXYZ

-- | uses abc instead of xyz
prettyABC :: (Integral a) => Matrix (Ratio a) -> String
prettyABC = showAsABC
