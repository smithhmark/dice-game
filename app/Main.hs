module Main where
import System.IO
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

--stringifyCell :: Cell -> String
--stringifyCell (Cell p d) = printf "%c-%d" (chr (p + ord 'a')) d

--stringifyBoard :: Int -> Vector Cell -> String
--stringifyBoard s b = concat ls
  --where idxs = [ x * s | x <- [0..(s - 1)] ]
        --ss = [ V.foldr (\c a -> stringifyCell c ++ " " ++ a) "\n" $ V.slice x s b | x  <- idxs ]
        --ps = [ concat $ (take (s - x) $ repeat "  ") | x <- [0..(s - 1)] ]
        --ls = zipWith (++) ps ss


main :: IO ()
main = do
  --brd <- evalRandIO $ randBoard 4 2 3
  --print brd
  --putStr $ stringifyBoard 2 brd
  putStr "How big a board do you want? "
  hFlush stdout
  sizeS <- getLine
  putStr "What should the maximum dice on a cell be? "
  hFlush stdout
  diceS <- getLine
  putStrLn "Currently we are limiting to 2 players"
  hFlush stdout
  let sz = read sizeS
      d = read diceS
      p = 2
      gs = GameSetup sz p d
  brd <- evalRandIO $ randBoard (sz*sz) p d

  let startingTree = buildTree gs brd 0 0 True Nothing
  playVsHuman gs startingTree

  --let silly = V.fromList [Cell 0 1,Cell 1 1,Cell 0 2, Cell 1 1]
  --let gs = GameSetup 2 2 3
  --let tree = buildTree gs silly 0 0 True Nothing
  --print tree
  --putStr $ stringifyTree gs tree 4
  --print "dummyBoard:"
  --putStr . stringifyBoard gs $ dummyBoard
  --print "attackTestBoard:"
  --putStr $ stringifyBoard gs $ attackTestBoard
  --print "attackTestBoard2:"
  --putStr $ stringifyBoard gs $ attackTestBoard2
  --print "attackTestBoard3:"
  --putStr $ stringifyBoard gs $ attackTestBoard3

