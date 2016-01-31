module Main where
import System.IO
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V
import Data.Char (ord, chr)
import Text.Printf

import DiceGame


main :: IO ()
main = do
  --brd <- evalRandIO $ randBoard 9 2 3
  --print brd
  --print " "
  --putStr $ stringifyBoard 2 brd
  putStr "How big a board do you want? "
  hFlush stdout
  sizeS <- getLine
  putStr "What should the maximum dice on a cell be? "
  hFlush stdout
  diceS <- getLine
  putStrLn "How many players?"
  hFlush stdout
  playerS <- getLine
  let sz = read sizeS :: Int
      d = read diceS :: Int
      p = read playerS :: Int
      gs = GameSetup p sz d
      l = sz * sz
  putStr $ "Creating a board that has " ++ show l ++ " cells with " ++ show p
    ++ " players and up to " ++ show d ++ " dice per cell\n"
  brd <- generateBoard gs

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

