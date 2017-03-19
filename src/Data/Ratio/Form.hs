module Data.Ratio.Form (
  Form(..),
  ReadForm,
  fromForm,
  ShowForm,
  showForm,
  ) where

import Control.Monad
import Data.Ratio

-- |
-- Simple rational form data.
--
data Form a = INT a | RATIO a a
                  deriving (Eq)

-- |
-- for convert rational form to some numeric.
--
-- >>> fromForm . read $ "1/2" :: Rational
-- 1 % 2
-- >>> map fromForm $ read "[1/2,1/3,1/4]" :: [Rational]
-- [1 % 2,1 % 3,1 % 4]
class ReadForm a where
  fromForm :: (Integral b,Read b) => Form b -> a

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

instance (Read a,Integral a) => Read (Form a) where
  readsPrec _ n = join . sequence [a'',b'',a',b',a,b] $ n
    where
      a'' n = do
        ("+",st0) <- lex n
        (numer,st1) <- lex st0
        ("/",st2) <- lex st1
        (denom,st3) <- lex st2
        return (RATIO (read numer) (read denom), st3)
      b'' n = do
        ("+",st0) <- lex n
        (numer,st1) <- lex st0
        return (INT (read numer), st1)
      a' n = do
        ("-",st0) <- lex n
        (numer,st1) <- lex st0
        ("/",st2) <- lex st1
        (denom,st3) <- lex st2
        return (RATIO ((read numer) * (-1)) (read denom), st3)
      b' n = do
        ("-",st0) <- lex n
        (numer,st1) <- lex st0
        return (INT $ (read numer) * (-1), st1)
      a n = do
        (numer, st0) <- lex n
        ("/", st1) <- lex st0
        (denom, st2) <- lex st1
        return (RATIO (read numer) (read denom), st2)
      b n = do
        (numer, st0) <- lex n
        return (INT (read numer), st0)

instance (Show a) => Show (Form a) where
  showsPrec _ (INT n) = showString $ show n
  showsPrec _ (RATIO n d) = showString $ (show n) ++ "/" ++ (show d)

instance Functor Form where
  fmap f (INT a) = INT (f a)
  fmap f (RATIO a b) = RATIO (f a) (f b)

instance ReadForm Float where
  fromForm = fromRational . fromForm
  
instance ReadForm Double where
  fromForm = fromRational . fromForm

instance ReadForm Int where
  fromForm form = truncate (fromForm form :: Rational)

instance ReadForm Integer where
  fromForm form = truncate (fromForm form :: Rational)

instance (Integral a,Read a) => ReadForm (Ratio a) where
  fromForm (INT a) = (fromIntegral a) % 1
  fromForm (RATIO a b) = (fromIntegral a) % (fromIntegral b)

instance ShowForm Float where
  showForm = show

instance ShowForm Double where
  showForm = show

instance ShowForm Int where
  showForm = show

instance ShowForm Integer where
  showForm = show

instance (Integral a,Show a) => ShowForm (Ratio a) where
  showForm a = if denominator a == 1 then show . numerator $ a
               else (show . numerator $ a) ++ "/" ++ (show . denominator $ a)
