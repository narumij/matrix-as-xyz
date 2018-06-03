{- |
Module      : Data.Ratio.Slash
Copyright   : (c) Jun Narumi 2018
License     : BSD3
Maintainer  : narumij@gmail.com
Stability   : experimental
Portability : ?

%ではなく、/で記述した分数を取り扱う

-}

module Data.Ratio.Slash (
  Slash(..),
  ) where

import Control.Monad (guard)
import Control.Applicative ((<|>))
import Data.Ratio
import Numeric

-- |
-- read instanceとして読み込み、getRatioで取り出す
--
-- >>> getRatio . read $ "1/2"
-- 1 % 2
--
-- read instanceなので配列の表現でもまとえて読むことができる
--
-- >>> map getRatio . read $ "[1/2,3/4,5/6]"
-- [1 % 2,3 % 4,5 % 6]
--
-- Ratioから直接生成する
--
-- >>> Slash (1 % 2)
-- 1/2
--
-- Ratioの配列からまとめて生成する
--
-- >>> map Slash [1%2,3%4,5%6]
-- [1/2,3/4,5/6]
--
newtype Slash a = Slash { getRatio :: Ratio a } deriving (Eq,Ord)

instance (Integral a) => Show (Slash a) where
  showsPrec _ (Slash n)
    | denominator n == 1
      = showSigned showInt 0 (numerator n)
    | otherwise
      = showSigned showInt 0 (numerator n) . showString "/" . showInt (denominator n)

instance (Integral a) => Read (Slash a) where
  readsPrec _ n = do
    ( ( n, d ), st ) <- readRatio n
    guard $ d /= 0
    return ( Slash $ n % d , st )

readInteger :: (Integral a) => ReadS (a,a)
readInteger n = do
  ( n, st ) <- readSigned readDec n
  return ( ( n, 1 ), st )

readRatio' :: (Integral a) => ReadS (a,a)
readRatio' n = do
  ( numer, st  )  <- readSigned readDec n
  ( "/"  , st1 ) <- lex st
  ( denom, st2 ) <- readDec st1
  return ( ( numer, denom ), st2)

readRatio :: (Integral a) => ReadS (a,a)
readRatio n = take 1 $ readRatio' n <|> readInteger n
