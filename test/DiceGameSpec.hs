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
      playerCells (GameSetup 2 2 3) dummyBoard 0 `shouldBe` [0, 1]
    it "finds cells owned by a player(attackTestBoard3)" $ do
      playerCells (GameSetup 2 2 3) dummyBoard 0 `shouldBe` [0, 1]

  describe "potentialTargets" $ do
    it "prevents attacking ones self" $ do
      potentialTargets 0 attackTestBoard3 [0,1] [[1,2,3], [0,3]] `shouldBe` 
        [(0, [2, 3]), (1,[3])]

  describe "winnable" $ do
    it "stripout attacks that cant win" $ do
      winnable 0 attackTestBoard3 [(0, [2, 3]), (1,[3])] `shouldBe` 
        [(0,[3]), (1, [3])]
    it "handels none" $ do
      winnable 0 dummyBoard [(0, [2, 3]), (1,[3])] `shouldBe` 
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
    it "adds dice to open cells" $ do
      addNewDice (GameSetup 2 2 3) 
                 (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1])
                 0
                 2
        `shouldBe`
                 V.fromList [Cell 0 2,Cell 1 3,Cell 0 3,Cell 1 1]
    it "adds dice to open cells, but not too many" $ do
      addNewDice (GameSetup 2 2 3) 
                 (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1])
                 0
                 1
        `shouldBe`
                 V.fromList [Cell 0 2,Cell 1 3,Cell 0 2,Cell 1 1]

  describe "playerCounts" $ do
    it "histograms a board by ownership" $ do
      playerCounts (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1]) []
        `shouldBe` [(2, 1), (2, 0)] 

  describe "winners" $ do
    it "builds a list of players with max ownership" $ do
      winners (V.fromList [Cell 1 1,Cell 1 3,Cell 0 2,Cell 1 1])
        `shouldBe` [1] 
    it "builds a list of players with max ownership (tie)" $ do
      winners (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1])
        `shouldBe` [1, 0] 
