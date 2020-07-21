module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest [
  "--fast",
  "src/Data/Ratio/Slash.hs",
  "src/Data/Ratio/ParseFloat.hs",
  "src/Data/Matrix/AsXYZ.hs"
  ]
