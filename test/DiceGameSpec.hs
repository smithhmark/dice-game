module DiceGameSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import DiceGame

import qualified Data.Vector as V

spec :: Spec
spec = do
  describe "nxtPlyr" $ do
    it "finds the player who's turn is next" $ do
      nxtPlyr 0 (GameSetup 2 42 3) `shouldBe` 1
    it "finds the player who's turn is next wrapping around" $ do
      nxtPlyr 32 (GameSetup 33 42 3) `shouldBe` 0

  describe "neighbors" $ do
    it "handles bottom-left positions" $ do
      neighbors (GameSetup 2 2 3) dummyBoard 2 `shouldBe` [0,3]
    it "handles top-left positions" $ do
      neighbors (GameSetup 2 2 3) dummyBoard 0 `shouldBe` [1,2,3]
    it "handles top-right row positions" $ do
      neighbors (GameSetup 2 2 3) dummyBoard 1 `shouldBe` [0,3]

  describe "playerCells" $ do
    it "finds cells owned by a player(dummyBoard)" $ do
      playerCells dummyBoard 0 `shouldBe` [0, 1]
    it "finds cells owned by a player(attackTestBoard3)" $ do
      playerCells dummyBoard 0 `shouldBe` [0, 1]

  describe "potentialTargets" $ do
    it "prevents attacking ones self" $ do
      potentialTargets 0 attackTestBoard3 [0,1] [[1,2,3], [0,3]] `shouldBe` 
        [(0, [2, 3]), (1,[3])]

  describe "winnable" $ do
    it "stripout attacks that cant win" $ do
      winnable attackTestBoard3 [(0, [2, 3]), (1,[3])] `shouldBe` 
        [(0,[3]), (1, [3])]
    it "handels none" $ do
      winnable dummyBoard [(0, [2, 3]), (1,[3])] `shouldBe` 
        [(0,[]), (1, [])]

  describe "attacks" $ do
    it "discovers sole attack" $ do
      attacks (GameSetup 2 2 3) attackTestBoard 0 `shouldBe` [ (0, 2) ]
    it "discovers all attacks" $ do
      attacks (GameSetup 2 2 3) attackTestBoard2 0 `shouldBe` 
        [ (0, 1), (0, 2), (0,3) ]
    it "handles zero attacks" $ do
      attacks (GameSetup 2 2 3) attackTestBoard2 1 `shouldBe` []

  describe "addNewDice" $ do
    context "where there are fewer dice than open cells" $ do
      it "adds dice to open cells" $ do
        addNewDice (GameSetup 2 2 3) 
                   (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1])
                   0
                   2
          `shouldBe`
                   V.fromList [Cell 0 2,Cell 1 3,Cell 0 3,Cell 1 1]
    context "where there are more dice than open cells" $ do
      it "adds dice to open cells, but not too many" $ do
        addNewDice (GameSetup 2 2 3) 
                   (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1])
                   0
                   1
          `shouldBe`
                   V.fromList [Cell 0 2,Cell 1 3,Cell 0 2,Cell 1 1]

  describe "playerCounts" $ do
    let t = V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1]
        v = V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 0 1]
    it "finds the tie" $ do
      playerCounts t [] `shouldBe` [(2, 1), (2, 0)] 
    it "finds player 0 has the edge" $ do
      playerCounts v [] `shouldBe` [(1, 1), (3, 0)] 

  describe "winners" $ do
    it "builds a list of players with max ownership" $ do
      winners (V.fromList [Cell 1 1,Cell 1 3,Cell 0 2,Cell 1 1])
        `shouldBe` [1] 
    it "builds a list of players with max ownership (tie)" $ do
      winners (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1])
        `shouldBe` [1, 0] 

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
