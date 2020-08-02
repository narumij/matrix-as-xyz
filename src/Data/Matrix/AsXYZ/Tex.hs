module Data.Matrix.AsXYZ.Tex where

import Control.Monad (join)

import Data.Char (isAlpha,toLower)
import Data.List
import Data.Ratio
import Data.Matrix (Matrix,fromList,fromLists,toLists,identity,zero,(<->))

import Numeric

import Data.Matrix.AsXYZ.Common

data Command = None | Space | SizeN | SizeS | Size Relative | Str String deriving (Show,Eq)

data Relative = Tiny | Scriptsize | Footnotesize | Small | Normalsize | Large | Huge deriving (Show,Eq)

label :: String -> String
label l = "\\" ++ l

curly :: String -> String
curly a = "{" ++ a ++ "}"

bar :: String -> String
bar a = label "overline" ++ curly a

num :: Integral a => Ratio a -> String
num r | d == 1 = int n
      | otherwise = frac n d
  where
    nn = numerator r
    d = denominator r
    n = if nn >= 0 then nn else nn * (-1)
    showInt' = flip showInt "" -- Constraint Show class を回避するため
    int n = showInt' n
    frac n d = label "frac" ++ (join . map (curly . showInt') $ [n,d])

texP label (P var) = [Str "+"] ++ texV label id var
texP label (N var) = [Str "-"] ++ texV label id var
texP _ Zero = [Str "0"]

texP' label (P var) = texV label id var
texP' label (N var) = texV label bar var
texP' _ Zero = [Str "0"]

texV _      bar (W n) = [SizeS, Str . bar . num $ n, SizeN]
texV labels bar var   = if flag then [Str . bar $ label] else [SizeS, Str . bar . num $ n, SizeN, Str label]
  where
    get (X n) = (0,n)
    get (Y n) = (1,n)
    get (Z n) = (2,n)
    t = get var
    n = snd t
    label = labels !! fst t:[]
    flag = 1 == n

reduceCommands :: [Command] -> [Command]
reduceCommands = f None None
  where
    f None None    (x@(Str n):xs) = Str n:f None x xs
    f None None    (x        :xs) = f None x xs
    f a    (Str _) (Str n    :xs) = (Str n) : f a (Str n) xs
    f a    b       (Str n    :xs) | a /= b    = b : Str n : f b (Str n) xs
                                  | otherwise = Str n : f b (Str n) xs
    f a    _       (x        :xs) = f a x xs
    f _    _       []             = []

reduceNormalsize :: [Command] -> [Command]
reduceNormalsize = f None
  where
    f None (Size Normalsize:xs) =   f None xs
    f None (x@(Size n):xs)      = x:f x xs
    f None (x:xs)               = x:f None xs
    f _    (x:xs)               = x:f x xs
    f _    []                   = []

renderCommand :: Relative -> Relative -> [Command] -> String
renderCommand sizeN sizeS = concatMap render . reduceNormalsize . (renderSize sizeN sizeS)
  where
    renderS SizeN = Size sizeN
    renderS SizeS = Size sizeS
    renderS a = a
    renderSize sizeN sizeS = map renderS
    render None = ""
    render (Str s) = s
    render (Size s) = "\\" ++ (map toLower $ show s) ++ " "

toCommands :: Integral a => [Char] -> [Sign (Var (Ratio a))] -> [Command]
toCommands label (x:xs) = [SizeN] ++ texP' label x ++ concatMap (texP label) xs

rowCommands :: Integral a => String -> [Ratio a] -> [Command]
rowCommands label = toCommands label . rowVars

commands :: Integral a => String -> [[Ratio a]] -> [Command]
commands label = intercalate [SizeN, Str ","] . map (rowCommands label)

rowsString :: Integral a => String -> Relative -> Relative -> [[Ratio a]] -> String
rowsString label sizeN sizeS = renderCommand sizeN sizeS . reduceCommands . commands label

texAs :: Integral a => String -> Relative -> Relative -> Matrix (Ratio a) -> String
texAs label sizeN sizeS = rowsString label sizeN sizeS . take 3 . toLists

