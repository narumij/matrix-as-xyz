module Data.Matrix.AsXYZ.Plain (
  showAs,
  xyzLabel,
  abcLabel,
)where

import Control.Monad (join)

import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.Ratio (Ratio)
import Data.Ratio.Slash (getRatio,Slash(..))
import Data.Matrix (Matrix,fromList,fromLists,toLists,identity,zero,(<->))

-- +または-が先頭に必ずあるようにする
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

row :: (Integral a) => [String] -> [Ratio a] -> String
row labels line = join . varSort $ zipWith varString line labels

refineRow :: String -> String
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
