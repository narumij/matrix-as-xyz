module FormSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding ((.&.))

import Data.Ratio.Form

spec :: Spec
spec = do
  describe "integer" $ do
    it "read of '1'" $
      read "1" `shouldBe` (INT 1)
      
    prop "reverse read" $ \n -> (read $ show (INT n) :: Form Integer) == (INT n)

  describe "rational" $ do
    it "read of '1/2'" $
      read "1/2" `shouldBe` (RATIO 1 2)

    prop "reverse read" $ \n m -> let mm = abs m
                                  in (read $ show (RATIO n mm) :: Form Integer) == (RATIO n mm)


