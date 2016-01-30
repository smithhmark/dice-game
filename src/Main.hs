module Main where

import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random
import Control.Monad.Random
import Data.Char (ord, chr)
import Text.Printf
import qualified Data.Vector as V

type Player = Int
data Cell = Cell {owner :: Player, dice :: Int} deriving (Show, Eq)
type Board = Vector Cell
type Rnd a = Rand StdGen a

data GameSetup = GameSetup { playerCnt :: Int
                           , boardSize :: Int
                           } deriving (Show)

data GameTree = GameTree { player :: Player
                         , board :: Board
                         --, spareDice :: Int
                         --, fstMv :: Bool
                         , moves :: [GameTree]
                         } deriving (Show, Eq)

buildTree :: GameSetup -> Board -> Player -> Int -> Bool -> GameTree
buildTree g brd plyr srdc fm =
  GameTree plyr brd $ addPassingMoves g brd plyr srdc fm $ addAttackingMoves g brd plyr srdc

nxtPlyr :: Player -> GameSetup -> Player
nxtPlyr p g = mod (p + 1) $ playerCnt g

addPassingMoves :: GameSetup -> Board -> Player -> Int -> Bool -> [GameTree] -> [GameTree]
addPassingMoves g brd plyr srdc True mvs = mvs
addPassingMoves g brd plyr srdc False mvs = 
   (buildTree g 
             (addNewDice brd plyr (srdc - 1)) 
             (nxtPlyr plyr g) 
             0 
             True) : mvs

addNewDice = undefined

playerAt :: Int -> Board -> Player
playerAt pos brd = owner $ brd ! pos

diceAt :: Int -> Board -> Int
diceAt pos brd = dice $ brd ! pos

ownedByP :: Player -> Cell -> Bool
ownedByP p c = p == owner c

neighbors :: GameSetup -> Board -> Int -> [Int]
neighbors g brd pos = filter (>= 0) . filter (< V.length brd) $ concat [g1, g2, g3]
  where sz = boardSize g
        up = pos - sz
        down = pos + sz
        leftEdgeP = case pos `mod` sz of 0 -> True
                                         _ -> False
        rightEdgeP = case (pos + 1) `mod` sz of 0 -> True
                                                _ -> False
        g1 = [ up , down]
        g2 = case leftEdgeP of False -> [up -1, pos -1]
                               True -> []
        g3 = case rightEdgeP of False -> [up + 1, pos + 1]
                                True -> []

attacks :: GameSetup -> Board -> Player -> [ (Int, Int) ]
attacks g b p = undefined
  where srcs = V.toList $ V.findIndices (ownedByP p) b
        ns = map (neighbors g b) srcs
        trgts = map (filter (\x-> not (ownedByP p (b ! x)))) ns

addAttackingMoves :: GameSetup -> Board -> Player -> Int -> [GameTree]
addAttackingMoves g b p sprd = undefined
  where ats = attacks g b p


randCell :: Int -> Int -> Rnd Cell
randCell p d = do
  i <- getRandomR (0, p) :: Rnd Int
  c <- getRandomR (0, d) :: Rnd Int
  return $ Cell i c

randBoard :: Int -> Int -> Int -> Rnd (Vector Cell)
randBoard l p d = V.replicateM l $randCell p d

stringifyCell :: Cell -> String
stringifyCell (Cell p d) = printf "%c-%d" (chr (p + ord 'a')) d

stringifyBoard :: Int -> Vector Cell -> String
stringifyBoard s b = concat ls
  where idxs = [ x * s | x <- [0..(s - 1)] ]
        ss = [ V.foldr (\c a -> stringifyCell c ++ " " ++ a) "\n" $ V.slice x s b | x  <- idxs ]
        ps = [ concat $ (take (s - x) $ repeat "  ") | x <- [0..(s - 1)] ]
        ls = zipWith (++) ps ss

dummyBoard = V.fromList [Cell 0 3, Cell 0 3, Cell 1 3, Cell 1 3 ]

main :: IO ()
main = do
  brd <- evalRandIO $ randBoard 4 2 3
  print brd
  putStr $ stringifyBoard 2 brd
  putStr $ stringifyBoard 2 $ dummyBoard

