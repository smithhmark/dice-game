module Main where
import System.IO
import Control.Monad.Reader

import DiceGame
import ConsolePlay

main :: IO ()
main = do
  let sz = 3
      d = 3
      gs = buildGS 2 sz d
  --brd <- generateBoard gs
  --print brd
  let cells = [Cell {owner = 0, dice = 3}
              ,Cell {owner = 0, dice = 2}
              ,Cell {owner = 0, dice = 2}
              ,Cell {owner = 0, dice = 2}
              ,Cell {owner = 1, dice = 1}
              ,Cell {owner = 1, dice = 3}
              ,Cell {owner = 0, dice = 1}
              ,Cell {owner = 1, dice = 3}
              ,Cell {owner = 1, dice = 3}
              ]
      brd = customBoard cells
      startingTree = runReader (buildTree brd 0 0 True Nothing) gs
  cpuVsCpu gs startingTree
