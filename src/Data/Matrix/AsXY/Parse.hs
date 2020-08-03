{- |
Module      : Data.Matrix.AsXYZ.Parse
Copyright   : (c) Jun Narumi 2018
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?
-}
module Data.Matrix.AsXY.Parse (
  P.Value,
  equivalentPositions,
  transformPpABC,
  transformQqXYZ,
  P.ratio,
  P.integral,
  P.floating,
  ) where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Text.ParserCombinators.Parsec
import qualified Data.Matrix.AsXYZ.Parse as P

xy :: CharParser () Char
xy = oneOf "xyXY"

ab :: CharParser () Char
ab = oneOf "abAB"

-- | General equivalent positions parser
equivalentPositions :: Num a =>
　　　　　　　　　　　　　　P.ReadNum a -- ^ use converter below
　　　　　　　　　　　　 -> CharParser () [[a]]
equivalentPositions = components xy

-- | Same as equivalentPositions but uses abc instead of xyz
transformPpABC :: Num a => P.ReadNum a -> CharParser () [[a]]
transformPpABC = components ab

-- | Alias of equivalentPositions
transformQqXYZ :: Num a => P.ReadNum a -> CharParser () [[a]]
transformQqXYZ = components xy

data Var a
  = X a
  | Y a
  | Z a
  deriving (Show,Eq)

instance Functor Var where
  fmap f (X a) = X (f a)
  fmap f (Y a) = Y (f a)
  fmap f (Z a) = Z (f a)

v c = f $ toLower <$> c
  where
    f (Just 'x') = X
    f (Just 'a') = X
    f (Just 'y') = Y
    f (Just 'b') = Y
    f Nothing = Z

one :: Num a => CharParser () Char -> P.ReadNum a -> CharParser () (Var a)
one var numRead = do
  s <- optionMaybe P.sign
  option () spaces
  (n,l) <- P.elementBody var numRead
  return $ v l . P.minus s . fromMaybe 1 $ n

other :: Num a => CharParser () Char -> P.ReadNum a -> CharParser () (Var a)
other var numRead = do
  s <- P.sign
  option () spaces
  (n,l) <- P.elementBody var numRead
  return $ v l . P.minus (Just s) . fromMaybe 1 $ n

constructRow :: Num a => [Var a] -> [a]
constructRow = map (fromMaybe 0 . listToMaybe . catMaybes) . transpose . map toArray
  where
    toArray (X n) = [Just n,Nothing,Nothing]
    toArray (Y n) = [Nothing,Just n,Nothing]
    toArray (Z n) = [Nothing,Nothing,Just n]

component :: Num b => CharParser () Char -> P.ReadNum b -> CharParser () [b]
component var numRead = do
  option () spaces
  x <- one var numRead
  xs <- many (other var numRead)
  option () spaces
  let mm = x : xs
  if P.overlap (map void mm)
    then
      fail "overlaps var type"
    else
      return (constructRow mm)

components :: Num a => CharParser () Char -> P.ReadNum a -> CharParser () [[a]]
components var conv = do
  a <- component var conv
  char ','
  b <- component var conv
  return [a,b]

