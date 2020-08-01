{- |
Module      : Data.Matrix.AsXYZ
Copyright   : (c) Jun Narumi 2017-2018
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

Read and Display matrix as represented likes 'x,y,z' called Jones-Faithful notation or coordinate triplet.

-}
module Data.Matrix.AsXY (
  fromXY,
  fromXY',
  fromAB,
  prettyXY,
  prettyAB,
  texXY,
  texAB,
  ) where

import Control.Monad (join)
import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.Ratio (Ratio,(%))
import Data.Matrix (Matrix,fromList,fromLists,toLists,identity,zero,(<->))
import Text.ParserCombinators.Parsec (parse,ParseError)

import Data.Ratio.Slash (getRatio,Slash(..))
import Data.Matrix.AsXY.Parse (equivalentPositions,transformPpABC,ratio)

import qualified Data.Matrix.AsXY.Plain as Plain
import qualified Data.Matrix.AsXY.Tex as Tex

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
fromXY :: Integral a => String -> Matrix (Ratio a)
fromXY input = unsafeGet $ makeMatrix <$> parse (equivalentPositions ratio) input input

-- | Maybe version
fromXY' :: Integral a => String -> Maybe (Matrix (Ratio a))
fromXY' input = get $ makeMatrix <$> parse (equivalentPositions ratio) input input

-- | It's uses abc instead of xyz
--
-- >                                      ( 1 % 1 0 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 1 % 1 0 % 1 0 % 1 )
-- >                                      ( 0 % 1 0 % 1 1 % 1 0 % 1 )
-- > fromXYZ "a,b,c" :: Matrix Rational = ( 0 % 1 0 % 1 0 % 1 1 % 1 )
fromAB :: Integral a => String -> Matrix (Ratio a)
fromAB input = unsafeGet $ makeMatrix <$> parse (transformPpABC ratio) input input

makeMatrix :: Num a => [[a]] -> Matrix a
makeMatrix m = fromLists m <-> fromLists [[0,0,1]]

unsafeGet :: Either ParseError a -> a
unsafeGet e = case e of
  Left s -> error $ show s
  Right m -> m

get :: Either ParseError a -> Maybe a
get e = case e of
  Left s -> Nothing
  Right m -> Just m

----------------------------------

-- | Get the xyz representation of matrix
--
-- >>> prettyXYZ (identity 4 :: Matrix Rational)
-- "x,y,z"
--
-- >           ( 0 % 1 0 % 1 0 % 1 1 % 2 )
-- >           ( 0 % 1 0 % 1 0 % 1 2 % 3 )
-- >           ( 0 % 1 0 % 1 0 % 1 4 % 5 )
-- > prettyXYZ ( 0 % 1 0 % 1 0 % 1 1 % 1 ) = "1/2,2/3,4/5"
prettyXY :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyXY = Plain.showAs Plain.xyzLabel


-- | It's uses abc instead of xyz as text format
--
-- >>> prettyABC (identity 4 :: Matrix Rational)
-- "a,b,c"
prettyAB :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyAB = Plain.showAs Plain.abcLabel

-- | Get the xyz representation of matrix as tex format
--
-- >>> texXYZ (identity 4 :: Matrix Rational)
-- "\\normalsize x,y,z"
--
-- \( \normalsize x,y,z \)
--
-- >>> texXYZ . fromLists $ [[1,0,0,-1%2],[-1,0,0,1%2],[-1,0,0,0]]
-- "\\normalsize x-\\small \\frac{1}{2}\\normalsize ,\\overline{x}+\\small \\frac{1}{2}\\normalsize ,\\overline{x}"
--
-- \( \normalsize x-\small \frac{1}{2}\normalsize ,\overline{x}+\small \frac{1}{2}\normalsize ,\overline{x} \)
--
-- >>> texXYZ . fromLists $ [[-1,0,0,0],[0,0,0,0],[1,0,0,0]]
-- "\\normalsize \\overline{x},0,x"
--
-- \( \normalsize \overline{x},0,x \)
--
-- >>> texXYZ . fromLists $ [[0,0,0,-1%4],[0,0,0,1%4],[0,0,0,-1%4]]
-- "\\small \\overline{\\frac{1}{4}}\\normalsize ,\\small \\frac{1}{4}\\normalsize ,\\small \\overline{\\frac{1}{4}}"
--
-- \( \small \overline{\frac{1}{4}}\normalsize ,\small \frac{1}{4}\normalsize ,\small \overline{\frac{1}{4}} \)
--
texXY :: Matrix Rational -> String
texXY = Tex.texAs "xyz" Tex.Normalsize Tex.Small


-- | It's uses abc instead of xyz
--
-- >>> texABC (identity 4 :: Matrix Rational)
-- "\\normalsize a,b,c"
--
-- \( \normalsize a,b,c \)
texAB :: Matrix Rational -> String
texAB = Tex.texAs "abc" Tex.Normalsize Tex.Small
