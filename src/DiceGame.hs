module DiceGame where

import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V

type Player = Int
data Cell = Cell {owner :: Player, dice :: Int} deriving (Show, Eq)
type Board = Vector Cell

data GameSetup = GameSetup { playerCnt :: Int
                           , boardSize :: Int
                           , maxDice :: Int
                           } deriving (Show)

data GameTree = GameTree { player :: Player
                         , board :: Board
                         , moves :: [GameTree]
                         } deriving (Show, Eq)

dummyBoard = V.fromList [Cell 0 3, Cell 0 3, Cell 1 3, Cell 1 3 ]
attackTestBoard = V.fromList [Cell 0 3, Cell 0 2, Cell 1 2, Cell 1 3 ]
attackTestBoard2 = V.fromList [Cell 0 3, Cell 1 2, Cell 1 2, Cell 1 1 ]
attackTestBoard3 = V.fromList [Cell 0 3, Cell 0 3, Cell 1 3, Cell 1 1 ]

buildTree :: GameSetup -> Board -> Player -> Int -> Bool -> GameTree
buildTree g brd plyr srdc fm =
  GameTree plyr brd $ addPassingMoves g brd plyr srdc fm $ addAttackingMoves g brd plyr srdc

nxtPlyr :: Player -> GameSetup -> Player
nxtPlyr p g = mod (p + 1) $ playerCnt g

addPassingMoves :: GameSetup -> Board -> Player -> Int -> Bool -> [GameTree] -> [GameTree]
addPassingMoves g brd plyr srdc True mvs = mvs
addPassingMoves g brd plyr srdc False mvs = 
   (buildTree g 
             (addNewDice g brd plyr (srdc - 1)) 
             (nxtPlyr plyr g) 
             0 
             True) : mvs

addNewDice :: GameSetup -> Board -> Player -> Int -> Board
addNewDice g b p d = 
  case d > spots of True -> undefined
                    False -> b // updates
  where pcs = playerCells g b p
        haveRoom = filter (\i-> maxDice g > diceAt i b) pcs
        spots = length haveRoom
        uB d (i:is) = (i, Cell p (diceAt i b + 1)):uB (d-1) is
        uB 0 _ = []
        uB _ [] = []
        updates = uB d haveRoom

playerAt :: Int -> Board -> Player
playerAt pos brd = owner $ brd ! pos

diceAt :: Int -> Board -> Int
diceAt pos brd = dice $ brd ! pos

ownedByP :: Player -> Cell -> Bool
ownedByP p c = p == owner c

neighbors :: GameSetup -> Board -> Int -> [Int]
neighbors g brd pos = filter (>= 0) . filter (< V.length brd) $ concat [g2, g3]
  where sz = boardSize g
        up = pos - sz
        down = pos + sz
        leftEdgeP = case pos `mod` sz of 0 -> True
                                         _ -> False
        rightEdgeP = case (pos + 1) `mod` sz of 0 -> True
                                                _ -> False
        g2 = case leftEdgeP of False -> [up -1, up, pos -1]
                               True -> [up]
        g3 = case rightEdgeP of False -> [pos + 1, down, down + 1]
                                True -> [down]

playerCells:: GameSetup -> Board -> Player -> [Int]
playerCells g b p = V.toList $ V.findIndices (ownedByP p) b

potentialTargets :: Player -> Board -> [Int] -> [[Int]] -> [(Int, [Int])]
potentialTargets p b srcs ns = zip srcs enemies
  where enemies = map (filter (\x-> not (ownedByP p (b ! x)))) ns

winnable :: Player -> Board -> [(Int, [Int])] -> [(Int, [Int])]
winnable p b pts = map (\(s, ds)->(s, filter (\d-> diceAt d b < diceAt s b) ds)) pts 

attacks :: GameSetup -> Board -> Player -> [ (Int, Int) ]
attacks g b p = concat $ map (\(s,ds)-> [(s, d)| d <- ds]) val
  where srcs = playerCells g b p
        ns = map (neighbors g b) srcs
        pts = potentialTargets p b srcs ns
        val = winnable p b pts 

addAttackingMoves :: GameSetup -> Board -> Player -> Int -> [GameTree]
addAttackingMoves g b p sprd = gts
  where ats = attacks g b p
        dice1 = map (\(s, _)-> diceAt s b) ats
        dice2 = map (\(_, d)-> diceAt d b) ats
        zd = zip3 ats dice1 dice2
        gts = [buildTree g (attackBoard b p a ds) p (sprd + dd) False | 
               (a, ds, dd) <- zd]

attackBoard :: Board -> Player -> (Int, Int) -> Int -> Board
attackBoard b p (src,dst) d = b // [sc, dc]
  where sc = (src, Cell p 1)
        dc = (dst, Cell p (d - 1))

