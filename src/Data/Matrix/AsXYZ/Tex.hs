module Data.Matrix.AsXYZ.Tex where

import Control.Monad (join)

import Data.Char (isAlpha,toLower)
import Data.List
import Data.Ratio
import Data.Matrix (Matrix,fromList,fromLists,toLists,identity,zero,(<->))

import Numeric

data Flag a = P a | N a | Zero deriving (Show)
data Var a = X a | Y a | Z a | W a deriving (Show)
data Command = None | Space | SizeN | SizeS | Str String deriving (Show,Eq)
data Size = Tiny | Scriptsize | Footnotesize | Small | Normalsize deriving Show

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

---------------------------

rowCommands label = reduceCommands . toCommands label . reduceVars . sortVars . toVars
  where
    
    toVars = zipWith (\a b -> hoge a b) [X,Y,Z,W]
      where
        hoge f r | r < 0 = N $ f (r * (-1))
                 | otherwise = P $ f r

    toCommands label (x:xs) = [SizeN] ++ texP' label x ++ concatMap (texP label) xs
      where
        toStr a = map toLower $ show a

    sortVars parts = filter isPrimary parts ++ filter (not . isPrimary) parts
      where
        -- 正の係数がついた変数である
        isPrimary :: Flag (Var a) -> Bool
        isPrimary Zero = False
        isPrimary (N _) = False
        isPrimary (P (W _)) = False
        isPrimary _ = True

    reduceVars rr = if null a then [Zero] else a
      where
        a = filter (not . isZero) rr

    isZero (N v) = isZero' v
    isZero (P v) = isZero' v
    isZero Zero = True

    isZero' (X n) = n == 0
    isZero' (Y n) = n == 0
    isZero' (Z n) = n == 0
    isZero' (W n) = n == 0

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

renderCommand :: Size -> Size -> [Command] -> String
renderCommand sizeN sizeS = concatMap render . removeHeadNormalsize . (renderSize sizeN sizeS)
  where
    renderS SizeN = cmd sizeN
    renderS SizeS = cmd sizeS
    renderS a = a
    cmd = Str . label . cmdText
    cmdText a = (++ " ") . map toLower $ show a
    renderSize sizeN sizeS = map renderS
    render None = ""
    render (Str s) = s
    removeHeadNormalsize (Str "\\normalsize ":a) = a
    removeHeadNormalsize b = b

commands label = intercalate [SizeN, Str ","] . map (rowCommands label)

string label sizeN sizeS = renderCommand sizeN sizeS . reduceCommands . commands label

rowString label sizeN sizeS
  = renderCommand sizeN sizeS . rowCommands label

texAs :: Integral a => String -> Size -> Size -> Matrix (Ratio a) -> String
texAs label sizeN sizeS = string label sizeN sizeS . take 3 . toLists

