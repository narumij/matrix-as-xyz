module FormSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))

import Data.Ratio
import Data.Ratio.Form

spec :: Spec
spec = do
  describe "integer" $ do
    prop "reverse" $ \n ->
      (read $ show (INT n)::Form) `shouldBe` (INT n)

  describe "rational" $ do
    prop "reverse" $ \n d->
      d > 0 ==> (read $ show (RATIO (n%d))::Form) `shouldBe` (RATIO (n%d))

  describe "float" $ do
    prop "reverse" $ \n->
      (read $ show (FLOAT (n))::Form) `shouldBe` (FLOAT n)


