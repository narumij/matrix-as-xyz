module Data.Matrix.AsXYZ (
  fromXYZ,
  toXYZ,
  ) where

import Data.Matrix
import Data.List
import Data.Char
import Data.Ratio.Form
  
rows :: String -> [String]
rows st = filter (/=",") $ groupBy (\x y -> x /= ',' && y /= ',') $ filter (not . isSpace) st

splitsAt :: [Int] -> [a] -> [[a]]
splitsAt [] xs = [xs]
splitsAt (i:is) xs = (take i xs) : splitsAt (map (subtract i) is) (drop i xs)

terms :: String -> [String]
terms r = filter (not . null) $ splitsAt indices r
  where indices = findIndices (\c -> c == '+' || c == '-') r

myHead :: Num a => [Maybe Int] -> Maybe a
myHead [] = Nothing
myHead m@(x : xs)
  | null m = Nothing
  | otherwise = case x of
      Nothing -> myHead xs
      Just xx -> Just (fromIntegral xx)

termIndex :: (Eq t,Num t) => String -> t
termIndex c = case m of Nothing -> 3
                        Just x -> x
  where lastIndex = (elemIndex . last) c
        labels = ["xyz","XYZ","abc","ABC"]
        m = myHead $ map lastIndex labels

findKey :: (Eq t,Integral t) => t -> [(t, String)] -> String
findKey _ [] = ""
findKey key ((k,v):xs)
  | key == k = v
  | otherwise = findKey key xs

row :: String -> [String]
row st = map (flip findKey t) ([0..3] :: [Integer])
  where t = map (\x->(termIndex x,x)) $ terms st

arrange :: String -> String
arrange s
    | any (flip elem "xXyYzZaAbBcC") s =
      if any (flip elem ['0' .. '9']) ss then ss else ss ++ ['1']
    | null s = "0"
    | otherwise = s
    where ss = init s

listXYZas4x3 :: String -> [[String]]
listXYZas4x3 str
  | length m < 3 = error $ "too few ',' in " ++ (show str)
  | length m > 3 = error $ "too many ',' in " ++ (show str)
  | otherwise = m
  where m = map (map arrange) $ map row $ rows str

listXYZas4x4 :: String -> [[String]]
listXYZas4x4 str = listXYZas4x3 str ++ [["0","0","0","1"]]

listsFromXYZ :: String -> [[String]]
listsFromXYZ = listXYZas4x4

----------------------------------

lists :: String -> [String]
lists s = concat $ listsFromXYZ s

readMatrixString :: String -> Matrix String
readMatrixString s = fromList 4 4 $ lists s

readAsXYZ :: (ReadForm a) => String -> Matrix a
readAsXYZ s = (fromForm . read') <$> readMatrixString s
  where read' :: String -> Form Int
        read' = read

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
fromXYZ = readAsXYZ

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

