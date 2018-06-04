{- |
Module      : Data.Matrix.AsXYZ.Parse
Copyright   : (c) Jun Narumi 2018
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?
-}
module Data.Matrix.AsXYZ.Parse (
  Value,
  equivalentPositions,
  transformPpABC,
  transformQqXYZ,
  ratio,
  integral,
  floating,
  ) where

import Control.Monad
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec

import Data.Ratio
import Data.Ratio.Slash

import Data.Ratio.ParseFloat (readFloatingPoint)

import Data.Matrix (fromList,fromLists,Matrix(..),joinBlocks,(<->))

-- | reader of 3 kind of number description to rational
--
-- equivalentPositionsに渡して使う
ratio :: Integral a => Value -> Either String (Ratio a)
ratio (I s) = Right $ getRatio . read $ s
ratio (R s) = Right $ getRatio . read $ s
ratio (F s) = Right $ readFloatingPoint s

-- | reader of integral number description to integral
--
-- equivalentPositionsに渡して使う
integral :: Integral a => Value -> Either String a
integral (I s) = Right $ fromIntegral (read s :: Integer)
integral (R s) = Left  $ "cannot convert to integer from " ++ s ++ "."
integral (F s) = Left  $ "cannot convert to integer from " ++ s ++ "."

-- | reader of 3 kind of number description to floating point
--
-- equivalentPositionsに渡して使う
floating :: Floating a => Value -> Either String a
floating v = fromRational <$> ratio v

data Val a
  = I a
  | F a
  | R a
  deriving Show

instance Functor Val where
  fmap f (I a) = I (f a)
  fmap f (F a) = F (f a)
  fmap f (R a) = R (f a)

-- | パーサーが使用する数値文字列を変換する際の中間表現
type Value = Val String

data Var a
  = X a
  | Y a
  | Z a
  | W a
  deriving (Show,Eq)

instance Functor Var where
  fmap f (X a) = X (f a)
  fmap f (Y a) = Y (f a)
  fmap f (Z a) = Z (f a)
  fmap f (W a) = W (f a)

v (Just 'x') = X
v (Just 'a') = X
v (Just 'y') = Y
v (Just 'b') = Y
v (Just 'z') = Z
v (Just 'c') = Z
v Nothing = W

sign :: CharParser () Char
sign = oneOf "-+"

zero :: CharParser () String
zero = do
  char '0'
  return "0"

num :: CharParser () String
num = do
  x <- oneOf "123456789"
  xs <- many digit
  return $ x : xs

int :: CharParser () String
int = zero <|> num

integer :: CharParser () Value
integer = do
  i <- int
  return (I i)

float :: CharParser () Value
float = do
  i <- option "" int
  char '.'
  f <- many digit
  return (F $ i ++ "." ++ f )

fract :: CharParser () Value
fract = do
  n <- many1 digit
  option () spaces
  char '/'
  option () spaces
  d <- many1 digit
  return (R $ n ++ "/" ++ d)

number' :: CharParser () Value
number'
  =   try fract
  <|> try float
  <|> integer

-- | numRead関数のシグネチャの簡易表記
type ReadNum b = Value -> Either String b

number :: ReadNum b -> CharParser () b
number numRead = do
  n <- number'
  case numRead n of
    Left s -> fail s
    Right nn -> return nn

elementBody :: CharParser () Char -> ReadNum a -> CharParser () (Maybe a, Maybe Char)
elementBody var conv = do
  n <- optionMaybe (number conv)
  option () spaces
  v <- optionMaybe var
  option () spaces
  guard (isJust n || isJust v)
  return (n,v)

minus :: Num a => Maybe Char -> (a -> a)
minus (Just '-') = negate
minus (Just '+') = id
minus Nothing    = id

one :: Num a => CharParser () Char -> ReadNum a -> CharParser () (Var a)
one var numRead = do
  s <- optionMaybe sign
  option () spaces
  (n,l) <- elementBody var numRead
  return $ v l . minus s . fromMaybe 1 $ n

other :: Num a => CharParser () Char -> ReadNum a -> CharParser () (Var a)
other var numRead = do
  s <- sign
  option () spaces
  (n,l) <- elementBody var numRead
  return $ v l . minus (Just s) . fromMaybe 1 $ n

overlap :: Eq a => [a] -> Bool
overlap n = (length . nub) n /= length n

constructRow :: Num a => [Var a] -> [a]
constructRow = map (fromMaybe 0 . listToMaybe . catMaybes) . transpose . map toArray
  where
    toArray (X n) = [Just n,Nothing,Nothing,Nothing]
    toArray (Y n) = [Nothing,Just n,Nothing,Nothing]
    toArray (Z n) = [Nothing,Nothing,Just n,Nothing]
    toArray (W n) = [Nothing,Nothing,Nothing,Just n]

component :: Num b => CharParser () Char -> ReadNum b -> CharParser () [b]
component var numRead = do
  option () spaces
  x <- one var numRead
  xs <- many (other var numRead)
  option () spaces
  let mm = x : xs
  if overlap (map void mm)
    then
      fail "overlaps var type"
    else
      return (constructRow mm)

components :: Num a => CharParser () Char -> ReadNum a -> CharParser () [[a]]
components var conv = do
  a <- component var conv
  char ','
  b <- component var conv
  char ','
  c <- component var conv
  return [a,b,c]

xyz :: CharParser () Char
xyz = oneOf "xyzXYZ"

abc :: CharParser () Char
abc = oneOf "abcABC"

-- | general equivalent positions parser
equivalentPositions :: Num a =>
　　　　　　　　　　　　　　ReadNum a -- ^ reader of numeric description
　　　　　　　　　　　　 -> CharParser () [[a]]
equivalentPositions = components xyz

-- | same as equivalentPositions but uses abc instead of xyz
transformPpABC :: Num a => ReadNum a -> CharParser () [[a]]
transformPpABC = components abc

-- | same as equivalentPositions
transformQqXYZ :: Num a => ReadNum a -> CharParser () [[a]]
transformQqXYZ = components xyz
