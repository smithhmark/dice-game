module Main where

import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V
import System.Random
import Control.Monad.Random
import Data.Char (ord, chr)
import Text.Printf

import DiceGame

type Rnd a = Rand StdGen a


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


main :: IO ()
main = do
  brd <- evalRandIO $ randBoard 4 2 3
  print brd
  putStr $ stringifyBoard 2 brd
  putStr $ stringifyBoard 2 $ dummyBoard
