module Main where

import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random
import Control.Monad.Random

import qualified Data.Vector as V

data Cell = Cell Int Int deriving (Show)

type Board = [Cell]

type Rnd a = Rand StdGen a

--randCell :: RandomGen -> Int -> Int -> Rnd Cell
randCell :: Int -> Int -> Rnd Cell
randCell p d = do
  i <- getRandomR (0, p) :: Rnd Int
  c <- getRandomR (0, d) :: Rnd Int
  return $ Cell i c

main :: IO ()
main = do
  g <- getStdGen
  print $ take 10 (randomRs ('a', 'z') g)
  --print $ take 10 (randoms g :: [Double])
  aCell <- evalRandIO $ randCell 2 3
  print aCell
  putStrLn "hello world"
