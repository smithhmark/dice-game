module Main where
import System.IO

import DiceGame
import ConsolePlay

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
                     gs = buildGS 2 sz d
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
                     gs = buildGS p sz d
                     l = sz * sz
                 putStr $ "Creating a board that has " ++ show l 
                   ++ " cells with " ++ show p
                   ++ " players and up to " ++ show d ++ " dice per cell\n"
                 brd <- generateBoard gs

                 let startingTree = buildTree gs brd 0 0 True Nothing
                 playVsHuman gs startingTree
