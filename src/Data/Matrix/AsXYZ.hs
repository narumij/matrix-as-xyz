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

import Control.Monad (join)
import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.Ratio (Ratio)
import Data.Matrix (Matrix,fromList,fromLists,toLists,identity,zero,(<->))
import Text.ParserCombinators.Parsec (parse,ParseError)

import Data.Ratio.Slash (getRatio,Slash(..))
import Data.Matrix.AsXYZ.Parse (equivalentPositions,transformPpABC,ratio)

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

-- | It's uses abc instead of xyz
--
-- >                                      ( 1 % 1 0 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 1 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 0 % 1 1 % 1 0 % 1 )
-- > fromXYZ "a,b,c" :: Matrix Rational = ( 0 % 1 0 % 1 0 % 1 1 % 1 )
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

-- +または-が銭湯に必ずあるようにする
addPlusSign :: String -> String
addPlusSign xs@('-':_) = xs
addPlusSign xs         = '+' : xs

-- 符号付きの数値文字列にする
numStr :: (Integral a) => Ratio a -> String
numStr = addPlusSign . show . Slash

varString :: (Integral a) => Ratio a -> String -> String
varString num label
　-- 0の場合省略
  | num == 0   = ""
  -- 4番目の項目で、変数が付かない場合、数値文字列化
  | null label = numStr num
  -- 数値が1で変数がある場合、数値を省略
  | num == 1   = "+" ++ label
  -- 数値が-1で変数がある場合、数値を省略
  | num == -1  = "-" ++ label
  -- それ以外では数値と変数を文字列化
  | otherwise  = numStr num ++ label

-- 正の係数がついた変数である
isPrimary :: String -> Bool
isPrimary x = (hasLetter . reverse) x && isPositive x

hasLetter :: String -> Bool
hasLetter (x:_) = isAlpha x
hasLetter _     = False

isPositive :: String -> Bool
isPositive ('+':_) = True
isPositive _       = False

-- 正の係数がついた変数を先頭にする
varSort :: [String] -> [String]
varSort parts = filter isPrimary parts ++ filter (not . isPrimary) parts

row labels line = join . varSort $ zipWith varString line labels

refineRow s
  -- 全ての項目が省略されていると空文字列になっているので、0
  | null s = "0"
  -- 先頭の項目が正の場合、+記号を省略できるので削る
  | head s == '+' = tail s
  | otherwise = s

rowString :: (Integral a) => [String] -> [Ratio a] -> String
rowString labels line = refineRow (row labels line)

xyzLabel :: [String]
xyzLabel = ["x","y","z",""]

abcLabel :: [String]
abcLabel = ["a","b","c",""]

showAs :: (Integral a) => [String] -> Matrix (Ratio a) -> String
showAs labels = intercalate "," . map (rowString labels) . take 3 . toLists


-- | Get the xyz coordinate string of matrix
--
-- >>> prettyXYZ (identity 4 :: Matrix Int)
-- "x,y,z"
--
-- >           ( 0 % 1 0 % 1 0 % 1 1 % 2 )
-- >           ( 0 % 1 0 % 1 0 % 1 2 % 3 )
-- >           ( 0 % 1 0 % 1 0 % 1 4 % 5 )
-- > prettyXYZ ( 0 % 1 0 % 1 0 % 1 1 % 1 ) = "1/2,2/3,4/5"
prettyXYZ :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyXYZ = showAs xyzLabel


-- | It's uses abc instead of xyz
--
-- >>> prettyXYZ (identity 4 :: Matrix Int)
-- "a,b,c"
prettyABC :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyABC = showAs abcLabel
