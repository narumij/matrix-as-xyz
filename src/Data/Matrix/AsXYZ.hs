module Data.Matrix.AsXYZ (
  fromXYZ,
  toXYZ,
  ) where

import Data.Char
import Data.Maybe
import Data.List
import Data.List.Split
import Data.Ratio.Form
import Data.Matrix
  
rowStr :: String -> [String]
rowStr = splitOn ","

elements :: String -> [String]
elements r = filter (not.null) $ split (keepDelimsL $ oneOf "-+") r

index :: String -> (Int,String)
index s
  | lastLetter `elem` concat letters = (head $ mapMaybe colPos letters,init s)
  | otherwise = (3,s)
  where lastLetter = last s
        colPos = elemIndex lastLetter
        letters = ["xyz","XYZ","abc","ABC"]
        
arrangeElement :: String -> [Maybe String]
arrangeElement st = map (`lookup` pairList) [0..3]
  where pairList = map index $ elements st
  
read' :: Int -> Maybe String -> Form
read' _  Nothing   = INT 0
read' n (Just "")  = INT (fromIntegral n)
read' _ (Just "+") = INT 1
read' _ (Just "-") = INT (-1)
read' _ (Just a)   = read a

readRow :: String -> [Form]
readRow = zipWith ($) (map read' [1,1,1,0]) . arrangeElement

elementList :: String -> [Form]
elementList str = concatMap readRow (rowStr str) ++ [INT 0,INT 0,INT 0,INT 1]

readMatrix :: String -> Matrix (Form)
readMatrix s = fromList 4 4 $ elementList s

-- |
-- Create a matirx from xyz coordinate string of spacegroup
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
fromXYZ :: (ReadForm a) => String -> Matrix a
fromXYZ s = fromForm <$> readMatrix s

----------------------------------

showRatio :: (ShowForm a) => a -> String
showRatio n
  | n < 0 = showForm n
  | otherwise = "+" ++ showForm n

showElem :: (ShowForm a) => (a, String) -> String
showElem (num,label)
  | num == 0 = ""
  | null label = showRatio num
  | num == 1 = "+" ++ label
  | num == -1 = "-" ++ label
  | otherwise = (showRatio num) ++ label

showPart :: (ShowForm a) => [String] -> [a] -> String
showPart st line
  | null s = "0"
  | head s == '+' = tail s
  | otherwise = s
  where parts = map showElem $ zip line st
        s = foldl1 (++) $ partSort parts

partSort :: [String] -> [String]
partSort parts = case find (\x->((not . null) x) && hasLetter x && isPositive x) parts of
  Just x -> [x] ++ filter (/=x) parts
  Nothing -> parts
  where
    hasLetter = isAlpha . last
    isPositive str = head str == '+'

xyzLabel :: [String]
xyzLabel = ["x","y","z",""]

showAs :: (ShowForm a) => [String] -> Matrix a -> String
showAs labels m = foldl1 (\x y-> x++","++y) $ map (showPart labels) $ take 3 $ toLists m

showAsXYZ :: (ShowForm a) => Matrix a -> String
showAsXYZ = showAs xyzLabel

-- | Get the xyz coordinate string of matrix
--
-- >>> toXYZ (identity 4 :: Matrix Int)
-- "x,y,z"
-- 
-- >       ( 0 % 1 0 % 1 0 % 1 1 % 2 )
-- >       ( 0 % 1 0 % 1 0 % 1 2 % 3 )
-- >       ( 0 % 1 0 % 1 0 % 1 4 % 5 )
-- > toXYZ ( 0 % 1 0 % 1 0 % 1 1 % 1 ) = "1/2,2/3,4/5"
toXYZ :: (ShowForm a) => Matrix a -> String
toXYZ = showAsXYZ

