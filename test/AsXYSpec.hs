module AsXYSpec where

import Control.Exception (evaluate)

import Test.Hspec
import Data.Ratio
import Data.Matrix
import Data.Matrix.AsXY

spec :: Spec
spec = do

   describe "Data.Matrix.AsXY.fromXY" $ do

     it "read empty throws exception" $ do
       evaluate (fromXY "") `shouldThrow` anyException

     it "read x,y" $ do
       fromXY "x,y" `shouldBe` (identity 3)

     it "read X,Y" $ do
       fromXY "X,Y" `shouldBe` (identity 3)

     it "read a,b" $ do
       fromAB "a,b" `shouldBe` (identity 3)

     it "read A,B" $ do
       fromAB "A,B" `shouldBe` (identity 3)

   describe "Data.Matrix.AsXYZ.prettyXYZ" $ do

     it "show 0 (2x2)" $ do
       prettyXY (zero 2 2) `shouldBe` "0,0"

     it "show 1 (2x2)" $ do
       prettyXY (identity 2) `shouldBe` "x,y"

     it "show 0 (3x3)" $ do
       prettyXY (zero 3 3) `shouldBe` "0,0"

     it "show 0 (3x4)" $ do
       prettyXY (zero 3 4) `shouldBe` "0,0"

     it "show 1 (2x3)" $ do
       prettyXY (submatrix 1 2 1 3 $ identity 3) `shouldBe` "x,y"

     it "show 1 (2x2)" $ do
       prettyXY (submatrix 1 2 1 2 $ identity 3) `shouldBe` "x,y"

     it "positive first" $ do
       prettyXY (fromLists [[1,-1,-1],[-1,1,-1]]) `shouldBe` "x-y-1,y-x-1"

     it "number last" $ do
       prettyXY (fromLists [[-1,-1,-1],[-1,-1,1]]) `shouldBe` "-x-y-1,-x-y+1"

   describe "Data.Matrix.AsXYZ.prettyABC" $ do

     it "show 0" $ do
       prettyAB (zero 4 4) `shouldBe` "0,0"

     it "show 1" $ do
       prettyAB (identity 4) `shouldBe` "a,b"
