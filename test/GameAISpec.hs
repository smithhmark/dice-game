module GameAISpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Vector as V

import DiceGame
import GameAI

spec :: Spec
spec = do
  describe "ratePosition" $ do
    context "total victory" $ do
      let b = V.fromList [Cell 0 1, Cell 0 1, Cell 0 1, Cell 0 1]
          zerosTurn = GameTree 0 b Nothing []
          onesTurn = GameTree 1 b Nothing []
      context "player 0's turn:" $ do
        it "should handle total victory" $ do
            ratePosition zerosTurn 0 `shouldBe` 1.0
        it "should detect total loss" $ do
            ratePosition onesTurn 1 `shouldBe` 0.0
      context "player 1's turn:" $ do
        it "should handle total victory" $ do
            ratePosition zerosTurn 0 `shouldBe` 1.0
        it "should detect total loss" $ do
            ratePosition onesTurn 1 `shouldBe` 0.0
    context "tie" $ do
      let b = V.fromList [Cell 0 1, Cell 0 1, Cell 1 1, Cell 1 1]
          turn = GameTree 0 b Nothing []
      it "should detect a tie" $ do
        ratePosition turn 0 `shouldBe` 0.5
    context "could win in one move" $ do
      let wBoard = V.fromList [Cell 0 1, Cell 0 1, Cell 0 1, Cell 1 1]
          tBoard = V.fromList [Cell 0 2, Cell 1 1, Cell 0 1, Cell 1 1]
          passGame = GameTree 1 tBoard Nothing []
          winGame = GameTree 0 wBoard (Just (0, 1)) []
          now = GameTree 0 tBoard Nothing [winGame, passGame]
      it "should rate this position as a win for player 0" $ do 
        ratePosition now 0 `shouldBe` 1.0
      it "should rate this position as a loss for player 1" $ do 
        ratePosition now 1 `shouldBe` 0.0

  describe "limitTreeDepth" $ do
    let bigTree = GameTree 0 (V.fromList []) Nothing [ 
                     GameTree 1 (V.fromList []) Nothing []
                   , GameTree 0 (V.fromList []) Nothing []
                   ]
    it "gives just root when asked" $ do
      limitTreeDepth bigTree 1 `shouldBe` GameTree 0 (V.fromList []) Nothing []
    it "gives the whole thing when asked" $ do
      limitTreeDepth bigTree 2 `shouldBe` bigTree
    it "handels \"underflow\"" $ do
      limitTreeDepth bigTree 3 `shouldBe` bigTree
      limitTreeDepth bigTree 2 `shouldBe` bigTree
