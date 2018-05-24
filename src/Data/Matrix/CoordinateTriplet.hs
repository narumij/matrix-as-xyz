module Data.Matrix.CoordinateTriplet (
  CoordTriplet(..),
  CoordinateTriplet,
  getMatrix4x4,
  read3x4S,
  showAsXYZ,
  showAsABC,
) where

import Control.Monad (guard)
import Control.Applicative ((<|>))
import Control.Arrow (first)
import Data.Char (isAlpha)
import Data.Ratio (Ratio)
import Data.Ratio.Slash -- (Slash(..))
import Data.Matrix (Matrix,fromList,toLists,(<->))
import Data.Maybe
import Data.List
import Data.List.Split

import Numeric

import Text.Read
import Text.Regex
import Text.Regex.Posix

import Text.ParserCombinators.ReadP as P
import Text.ParserCombinators.ReadPrec

newtype Elem a = Elem { getElem :: Ratio a } deriving (Show,Eq)

readsRat :: (Integral a) => ReadS (Elem a)
readsRat n = do
  (r,st) <- reads n
  return (Elem $ getRatio r,st)

readsFlt :: (Integral a) => ReadS (Elem a)
readsFlt n = do
  guard (n =~ "^[1-9]+[0-9]*\\.[0-9]*")
  (f,st) <- readSigned readFloat n
  return (Elem . realToFrac $ f,st)

instance (Integral a) => Read (Elem a) where
   readsPrec _ n = readsRat n <|> readsFlt n

rows :: String -> [String]
rows = splitOn ","

elements :: String -> [String]
elements r = filter (not.null) $ split (keepDelimsL $ oneOf "-+") r

readElement :: (Integral a) => ReadS (Ratio a)
readElement ""  = [(1,"")]
readElement "+" = [(1,"")]
readElement "-" = [(-1,"")]
readElement s = take 1 $ r0 s <|> r1 s
  where
    r0 s = do
      (num,st) <- reads s
      return (getElem num,st)
    r1 s = do
      ("+",st) <- lex s
      (num,st2) <- reads st
      return (getElem num,st2)

vars :: [String]
vars = ["xyz","XYZ","abc","ABC"]

condition :: String -> Bool
condition s = isAlpha (last s) && last s `elem` concat vars

assignIndex :: String -> Int
assignIndex s = if condition s then idx else 3
    where
      colPos = elemIndex $ last s
      idx = head $ mapMaybe colPos vars

numStr :: String -> String
numStr s = if condition s then init s else s

assoc :: (Integral a) => String -> Maybe (Int,Ratio a)
assoc s | null e = Nothing
        | otherwise = Just $ head e
  where
    e = [ (assignIndex s, n) | (n,"") <- readElement . numStr $ s ]

assocList :: (Integral a) => String -> Maybe [(Int,Ratio a)]
assocList st | all isJust a = Just (map fromJust a)
             | otherwise = Nothing
  where
    a = map assoc $ elements st

rowElements :: (Integral a) => String -> Maybe [Ratio a]
rowElements st = pickup <$> assocList st
  where
    pickup a = map (fromMaybe 0 . (`lookup` a)) [0..3]

allElements :: (Integral a) => String -> Maybe [Ratio a]
allElements s = do
  let r = rows s
  let rr = map (rowElements . removeSpace) r
  guard ((not.null) s)
  guard (length r == 3)
  guard ((not . any isNothing) rr)
  return (concatMap fromJust rr)

removeSpace :: String -> String
removeSpace = concat . splitOn " "

-------------------------------------

showRatio :: (Integral a) => Ratio a -> String
showRatio n | head s == '-' = s
            | otherwise = "+" ++ s
  where s = show $ Slash n

showElem :: (Integral a) => Ratio a -> String -> String
showElem num label | num == 0 = ""
                   | null label = showRatio num
                   | num == 1 = "+" ++ label
                   | num == -1 = "-" ++ label
                   | otherwise = showRatio num ++ label

showRow :: (Integral a) => [String] -> [Ratio a] -> String
showRow st line | null s = "0"
                 | head s == '+' = tail s
                 | otherwise = s
  where
     parts = zipWith showElem line st
     s = foldl1 (++) $ partSort parts


partSort :: [String] -> [String]
partSort parts = case find (\x->(not . null) x && hasLetter x && isPositive x) parts of
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
showAs labels m = foldl1 (\x y-> x++","++y) $ map (showRow labels) $ take 3 $ toLists m

showAsXYZ :: (Integral a) => Matrix (Ratio a) -> String
showAsXYZ = showAs xyzLabel

showAsABC :: (Integral a) => Matrix (Ratio a) -> String
showAsABC = showAs abcLabel

------------------

read3x4S :: Integral a => Int -> ReadS (Matrix (Ratio a))
read3x4S _ s = do
  (s,t) <- readStringS s
  ss <- maybeToList $ fromList 3 4 <$> allElements s
  return (ss,t)

------------------

showsPrecP :: (a -> String) -> Int -> a -> ShowS
showsPrecP p _ v s =  "'" ++ p v ++ "'" ++ s

data CoordTriplet a
  = XYZ { getMatrix3x4 :: Matrix (Ratio a) }
  | ABC { getMatrix3x4 :: Matrix (Ratio a) }
  deriving (Eq)

type CoordinateTriplet = CoordTriplet Integer

getMatrix4x4 :: (Integral a) => CoordTriplet a -> Matrix (Ratio a)
getMatrix4x4 t = getMatrix3x4 t <-> fromList 1 4 [0,0,0,1]

instance (Integral a) => Read (CoordTriplet a) where
  readsPrec n s = (first XYZ <$> r xyzRegex n s) <|> (first ABC <$> r abcRegex n s)
    where
      xyzRegex = "^[ 0-9xyzXYZ.,/+-]+$"
      abcRegex = "^[ 0-9abcABC.,/+-]+$"
      r :: Integral a => String -> Int -> ReadS (Matrix (Ratio a))
      r regexStr _ s = do
        (s,t) <- readStringS s
        guard (s =~ regexStr)
        ss <- maybeToList $ fromList 3 4 <$> allElements s
        return (ss,t)

instance (Integral a) => Show (CoordTriplet a) where
  showsPrec n (XYZ m) s = showsPrecP showAsXYZ n m s
  showsPrec n (ABC m) s = showsPrecP showAsABC n m s

asABC :: CoordTriplet a -> CoordTriplet a
asABC (XYZ m) = ABC m
asABC a@(ABC _) = a

asXYZ :: CoordTriplet a -> CoordTriplet a
asXYZ x@(XYZ _) = x
asXYZ (ABC m) = XYZ m

-------------------

readStringS :: ReadS String
readStringS n = readSQuoteS n <|> readDQuoteS n <|> readTillEnd2S n <|> readTillEndS n

readSQuoteS :: ReadS String
readSQuoteS s = do
  (s,t) <- matchAndAfter <$> maybeToList (matchRegexAll (mkRegex $ "^'" ++ charR ++ "+'") s)
  return ((init . tail) s ,t )

readDQuoteS :: ReadS String
readDQuoteS s = do
  (s,t) <- matchAndAfter <$> maybeToList (matchRegexAll (mkRegex $ "^\"" ++ charR ++ "+\"") s)
  return ((init . tail) s, t)

readTillEnd2S :: ReadS String
readTillEnd2S s = do
    (s,t) <- matchAndAfter <$> maybeToList (matchRegexAll (mkRegex tillEnd2) s)
    return (init s, last s : t)

readTillEndS :: ReadS String
readTillEndS s = maybeToList $ matchAndAfter <$> matchRegexAll (mkRegex tillEnd) s

charR = "[ 0-9xyzXYZabcABC.,/+-]"

charR' = "[ 0-9xyzXYZabcABC./+-]"

till3rdComma = "^" ++ connectR "," (replicate 3 (charR' ++ "+")) ++ ","

tillEnd = "^" ++ connectR "," (replicate 3 (charR' ++ "+")) ++ "$"

tillEnd2 = "^" ++ connectR "," (replicate 3 (charR' ++ "+")) ++ "[^ 0-9xyzXYZabcABC./+-]"

matchAndAfter (_,a,b,_) = (a,b)

connectR s = foldl1 (\a b -> a ++ s ++ b)

--------
