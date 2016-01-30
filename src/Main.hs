module Main where

import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random
import Control.Monad.Random
import Data.Char (ord, chr)
import Text.Printf
import qualified Data.Vector as V

type Player = Int
data Cell = Cell Player Int deriving (Show)
type Board = Vector Cell
type Rnd a = Rand StdGen a

data GameTree = GameTree { board :: Board
                         , player :: Player
                         , moves :: [GameTree]
                         , fstMv :: Bool
                         }

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

