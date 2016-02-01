module Main where
import System.IO
import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V
import Data.Char (ord, chr)
import Text.Printf

import DiceGame


main :: IO ()
main = do
  putStr "How big a board do you want? "
  hFlush stdout
  sizeS <- getLine
  putStr "What should the maximum dice on a cell be? "
  hFlush stdout
  diceS <- getLine
  putStrLn "playing against the computer?"
  hFlush stdout
  cpuS <- getLine
  case cpuS of "y" -> do 
                 let sz = read sizeS :: Int
                     d = read diceS :: Int
                     gs = GameSetup 2 sz d
                     l = sz * sz
                 brd <- generateBoard gs
                 let startingTree = buildTree gs brd 0 0 True Nothing
                 playVsComputer gs startingTree
               _ -> do
                 putStrLn "How many players?"
                 hFlush stdout
                 playerS <- getLine
                 let sz = read sizeS :: Int
                     d = read diceS :: Int
                     p = read playerS :: Int
                     gs = GameSetup p sz d
                     l = sz * sz
                 putStr $ "Creating a board that has " ++ show l 
                   ++ " cells with " ++ show p
                   ++ " players and up to " ++ show d ++ " dice per cell\n"
                 brd <- generateBoard gs

                 let startingTree = buildTree gs brd 0 0 True Nothing
                 playVsHuman gs startingTree
