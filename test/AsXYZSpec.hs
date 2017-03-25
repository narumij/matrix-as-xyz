module AsXYZSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))

import Data.Ratio
import Data.Matrix
import Data.Matrix.AsXYZ

spec :: Spec
spec = do
  describe "AsXYZ" $ do
    prop "reverse test Matrix Int" $ \a b c d e f g h i j k l->
      let m = fromList 4 4 [a,b,c,d, e,f,g,h, i,j,k,l, 0,0,0,1] :: Matrix Int
      in (fromXYZ $ toXYZ m) `shouldBe` m
  describe "AsXYZ" $ do
    prop "reverse test Matrix Rational" $ \a b c d e f g h i j k l->
      let m = fromList 4 4 [a,b,c,d, e,f,g,h, i,j,k,l, 0,0,0,1] :: Matrix Rational
      in (fromXYZ $ toXYZ m) `shouldBe` m



