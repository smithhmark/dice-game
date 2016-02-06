module Main where
import System.IO

import DiceGame
import ConsolePlay

main :: IO ()
main = do
  let sz = 3
      d = 3
      gs = buildGS 2 sz d
  brd <- generateBoard gs
  let startingTree = buildTree gs brd 0 0 True Nothing
  cpuVsCpu gs startingTree
