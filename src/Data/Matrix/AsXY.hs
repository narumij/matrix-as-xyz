{- |
Module      : Data.Matrix.AsXYZ
Copyright   : (c) Jun Narumi 2017-2018
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

Read and Display Jones-Faithfull notation for planegroup (e.g. 'x,y')

-}
module Data.Matrix.AsXY (
  fromXY,
  fromXY',
  fromAB,
  prettyXY,
  prettyAB,
  ) where

import Control.Monad (join)
import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.Ratio (Ratio,(%))
import Data.Matrix (Matrix,fromList,fromLists,toLists,identity,zero,(<->))
import Text.ParserCombinators.Parsec (parse,ParseError)

import Data.Ratio.Slash (getRatio,Slash(..))
import qualified Data.Matrix.AsXYZ.ParseXY as XY (equivalentPositions,transformPpABC,ratio)

import qualified Data.Matrix.AsXYZ.Plain as Plain

-- | Create a matirx from xyz coordinate string of general equivalent position
fromXY :: Integral a => String -> Matrix (Ratio a)
fromXY input = unsafeGet $ makeMatrix' <$> parse (XY.equivalentPositions XY.ratio) input input

-- | Maybe version
fromXY' :: Integral a => String -> Maybe (Matrix (Ratio a))
fromXY' input = get $ makeMatrix' <$> parse (XY.equivalentPositions XY.ratio) input input

-- | It's uses abc instead of xyz
fromAB :: Integral a => String -> Matrix (Ratio a)
fromAB input = unsafeGet $ makeMatrix' <$> parse (XY.transformPpABC XY.ratio) input input

makeMatrix' :: Num a => [[a]] -> Matrix a
makeMatrix' m = fromLists m <-> fromLists [[0,0,1]]

unsafeGet :: Either ParseError a -> a
unsafeGet e = case e of
  Left s -> error $ show s
  Right m -> m

get :: Either ParseError a -> Maybe a
get e = case e of
  Left s -> Nothing
  Right m -> Just m

-- | Get the xyz representation of matrix
prettyXY :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyXY = Plain.showAs' Plain.xyzLabel


-- | It's uses abc instead of xyz as text format
--
-- >>> prettyABC (identity 4 :: Matrix Rational)
-- "a,b,c"
prettyAB :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyAB = Plain.showAs' Plain.abcLabel

