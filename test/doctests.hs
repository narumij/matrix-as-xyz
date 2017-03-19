module Main where

import Test.DocTest

main :: IO ()
main = doctest ["src/Data/Ratio/Form.hs",
                "src/Data/Matrix/AsXYZ.hs"]
