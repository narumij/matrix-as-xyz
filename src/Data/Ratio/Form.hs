module Data.Ratio.Form (
  Form(..),
  ReadForm,
  fromForm,
  ShowForm,
  showForm,
  ) where


import Numeric
import Control.Applicative
import Data.Ratio


-- |
-- temporary Number
--
data Form = INT Integer | RATIO Rational | FLOAT Double
                  deriving (Eq)


-- |
-- for convert rational form to some numeric.
--
-- >>> fromForm . read $ "1/2" :: Rational
-- 1 % 2
-- >>> map fromForm $ read "[1/2,1/3,1/4]" :: [Rational]
-- [1 % 2,1 % 3,1 % 4]
class ReadForm a where
  fromForm :: Form -> a


-- |
-- show rational as simple slash form
--
-- >>> showForm $ 1 % 2
-- "1/2"
-- >>> showForm 0.5
-- "0.5"
-- >>> showForm 1
-- "1"
class (Num a,Ord a) => ShowForm a where
  showForm :: a -> String


readsInt :: Int -> ReadS Form
readsInt d n = do
  (i,st) <- readSigned readDec n
  return (INT i,st)

readsRat :: Int -> ReadS Form
readsRat d n = do
  (numer,st) <- readSigned readDec n
  ("/",st1) <- lex st
  (denom,st2) <- readDec st1
  return (RATIO (numer%denom),st2)

readsFlt :: Int -> ReadS Form
readsFlt d n = do
  (flt,st) <- readSigned readFloat n
  return (FLOAT flt,st)

readsNumber :: Int -> ReadS Form
readsNumber d n = take 1 $ readsRat d n <|> readsInt d n <|> readsFlt d n

readsPlusNumber :: Int -> ReadS Form
readsPlusNumber d n = do
  ("+",st) <- lex n
  readsNumber d st


instance Read Form where
  readsPrec d n = readsPlusNumber d n <|> readsNumber d n
  
instance Show Form where
  showsPrec _ (INT n) = showString $ show n
  showsPrec _ (RATIO n) = showString $ (show (numerator n)) ++ "/" ++ (show (denominator n))
  showsPrec _ (FLOAT n) = showString (show n)


instance ReadForm Float where
  fromForm = fromRational . fromForm
  
instance ReadForm Double where
  fromForm = fromRational . fromForm

instance ReadForm Int where
  fromForm form = truncate (fromForm form :: Rational)

instance ReadForm Integer where
  fromForm form = truncate (fromForm form :: Rational)

instance (Integral a) => ReadForm (Ratio a) where
  fromForm (INT a) = fromIntegral a % 1
  fromForm (RATIO a) = fromIntegral (numerator a) % fromIntegral (denominator a)
  fromForm (FLOAT a) = realToFrac a


instance ShowForm Float where
  showForm = show

instance ShowForm Double where
  showForm = show

instance ShowForm Int where
  showForm = show

instance ShowForm Integer where
  showForm = show

instance (Integral a,Show a) => ShowForm (Ratio a) where
  showForm a = if denominator a == 1
               then show . numerator $ a
               else (show . numerator $ a) ++ "/" ++ (show . denominator $ a)

