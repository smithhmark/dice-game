module DiceGameSpec (spec) where

import DiceGame

import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
  describe "nxtPlyr" $ do
    it "finds the player who's turn is next" $ do
      nxtPlyr 0 (GameSetup 2 42) `shouldBe` 1
    it "finds the player who's turn is next wrapping around" $ do
      nxtPlyr 32 (GameSetup 33 42) `shouldBe` 0

  describe "neighbors" $ do
    it "handles bottom-left positions" $ do
      neighbors (GameSetup 2 2) dummyBoard 2 `shouldBe` [0,3]
    it "handles top-left positions" $ do
      neighbors (GameSetup 2 2) dummyBoard 0 `shouldBe` [1,2,3]
    it "handles top-right row positions" $ do
      neighbors (GameSetup 2 2) dummyBoard 1 `shouldBe` [0,3]

