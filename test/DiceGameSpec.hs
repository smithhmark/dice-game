module DiceGameSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import DiceGame

import Control.Monad.Reader
import qualified Data.Vector as V
import qualified Data.Map.Strict as M
import qualified Data.List as L

spec :: Spec
spec = do
  describe "nxtPlyr" $ do
    it "finds the player who's turn is next" $ do
      nxtPlyr 0 (buildGS 2 42 3) `shouldBe` 1
    it "finds the player who's turn is next wrapping around" $ do
      nxtPlyr 32 (buildGS 33 42 3) `shouldBe` 0

  describe "neighbors" $ do
    it "handles bottom-left positions" $ do
      neighbors 2 2 `shouldBe` [0,3]
    it "handles top-left positions" $ do
      neighbors 2 0 `shouldBe` [1,2,3]
    it "handles top-right row positions" $ do
      neighbors 2 1 `shouldBe` [0,3]

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
      runReader (attacks attackTestBoard 0) (buildGS 2 2 3) 
        `shouldBe` [ (0, 2) ]
    it "discovers all attacks" $ do
      runReader (attacks attackTestBoard2 0) (buildGS 2 2 3)
        `shouldBe` [ (0, 1), (0, 2), (0,3) ]
    it "handles zero attacks" $ do
      runReader (attacks attackTestBoard2 1) (buildGS 2 2 3) `shouldBe` []

  describe "addNewDice" $ do
    context "where there are fewer dice than open cells" $ do
      it "adds dice to open cells" $ do
        runReader (addNewDice 
                  (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1])
                  0
                  2) (buildGS 2 2 3) 
          `shouldBe`
                   V.fromList [Cell 0 2,Cell 1 3,Cell 0 3,Cell 1 1]
    context "where there are more dice than open cells" $ do
      it "adds dice to open cells, but not too many" $ do
        runReader (addNewDice 
                   (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1])
                   0
                   1) (buildGS 2 2 3) 
          `shouldBe`
                   V.fromList [Cell 0 2,Cell 1 3,Cell 0 2,Cell 1 1]

  describe "territoryCount" $ do
    let t = V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1]
        v = V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 0 1]
    it "finds the tie" $ do
      territoryCount t `shouldBe` M.fromList [(1, 2), (0, 2)] 
    it "finds player 0 has the edge" $ do
      territoryCount v `shouldBe` M.fromList [(1, 1), (0, 3)] 

  describe "winners" $ do
    it "builds a list of players with max ownership" $ do
      winners (V.fromList [Cell 1 1,Cell 1 3,Cell 0 2,Cell 1 1])
        `shouldBe` [1] 
    it "builds a list of players with max ownership (tie)" $ do
      L.sort (winners (V.fromList [Cell 0 1,Cell 1 3,Cell 0 2,Cell 1 1]))
        `shouldBe` (L.sort [1, 0])

