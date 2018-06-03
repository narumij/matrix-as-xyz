module AsXYZSpec where

import Control.Exception (evaluate)

import Test.Hspec
import Data.Ratio
import Data.Matrix
import Data.Matrix.AsXYZ

spec :: Spec
spec = do

   describe "Data.Matrix.AsXYZ.fromXYZ" $ do

     it "read empty throws exception" $ do
       evaluate (fromXYZ "") `shouldThrow` anyException

     it "read x,y,z" $ do
       fromXYZ "x,y,z" `shouldBe` (identity 4)
