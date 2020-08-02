{- |
Module      : Data.Matrix.AsXYZ
Copyright   : (c) Jun Narumi 2017-2018
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

Read and Display matrix as represented likes 'x,y,z' called Jones-Faithful notation or coordinate triplet.

-}
module Data.Matrix.AsXYZ (
  fromXYZ,
  fromXYZ',
  fromABC,
  prettyXYZ,
  prettyABC,
  texXYZ,
  texABC,
  ) where

import Control.Monad (join)
import Data.Char (isAlpha)
import Data.List (intercalate)
import Data.Ratio (Ratio,(%))
import Data.Matrix (Matrix,fromList,fromLists,toLists,identity,zero,(<->))
import Text.ParserCombinators.Parsec (parse,ParseError)

import Data.Ratio.Slash (getRatio,Slash(..))
import Data.Matrix.AsXYZ.Parse (equivalentPositions,transformPpABC,ratio)

import qualified Data.Matrix.AsXYZ.Plain as Plain
import qualified Data.Matrix.AsXYZ.Tex as Tex (texAs,Relative(..))

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

-- | Get the xyz representation of matrix
--
-- >>> prettyXYZ (identity 4 :: Matrix Rational)
-- "x,y,z"
--
-- >           ( 0 % 1 0 % 1 0 % 1 1 % 2 )
-- >           ( 0 % 1 0 % 1 0 % 1 2 % 3 )
-- >           ( 0 % 1 0 % 1 0 % 1 4 % 5 )
-- > prettyXYZ ( 0 % 1 0 % 1 0 % 1 1 % 1 ) = "1/2,2/3,4/5"
prettyXYZ :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyXYZ = Plain.showAs Plain.xyzLabel


-- | It's uses abc instead of xyz as text format
--
-- >>> prettyABC (identity 4 :: Matrix Rational)
-- "a,b,c"
prettyABC :: (Integral a) =>
             Matrix (Ratio a) -- ^ 3x3, 3x4 or 4x4 matrix
          -> String
prettyABC = Plain.showAs Plain.abcLabel

-- | Get the xyz representation of matrix as tex format
--
-- >>> texXYZ (identity 4 :: Matrix Rational)
-- "x,y,z"
--
-- \( x,y,z \)
--
-- >>> texXYZ . fromLists $ [[1,0,0,-1%2],[-1,0,0,1%2],[-1,0,0,0]]
-- "x-\\small \\frac{1}{2}\\normalsize ,\\overline{x}+\\small \\frac{1}{2}\\normalsize ,\\overline{x}"
--
-- \( x-\small \frac{1}{2}\normalsize ,\overline{x}+\small \frac{1}{2}\normalsize ,\overline{x} \)
--
-- >>> texXYZ . fromLists $ [[-1,0,0,0],[0,0,0,0],[1,0,0,0]]
-- "\\overline{x},0,x"
--
-- \( \overline{x},0,x \)
--
-- >>> texXYZ . fromLists $ [[0,0,0,-1%4],[0,0,0,1%4],[0,0,0,-1%4]]
-- "\\small \\overline{\\frac{1}{4}}\\normalsize ,\\small \\frac{1}{4}\\normalsize ,\\small \\overline{\\frac{1}{4}}"
--
-- \( \small \overline{\frac{1}{4}}\normalsize ,\small \frac{1}{4}\normalsize ,\small \overline{\frac{1}{4}} \)
--
texXYZ :: Matrix Rational -> String
texXYZ = Tex.texAs "xyz" Tex.Normalsize Tex.Small


-- | It's uses abc instead of xyz
--
-- >>> texABC (identity 4 :: Matrix Rational)
-- "a,b,c"
--
-- \( a,b,c \)
texABC :: Matrix Rational -> String
texABC = Tex.texAs "abc" Tex.Normalsize Tex.Small

