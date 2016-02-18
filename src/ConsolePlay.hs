module ConsolePlay where

import Text.Printf
import Data.Char (ord, chr)
import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

import DiceGame
import GameAI

-- | prints the current player and the current board
printInfo :: GameSetup -> GameTree -> IO ()
printInfo g t = do
  putStrLn ""
  putStrLn $ printf "current player = %s" $ playerLabel $ player t
  putStr . stringifyBoard g $ board t

-- | determines the winner(s) then prints the result
announceWinner :: Board -> IO ()
announceWinner b = do
  putStrLn ""
  let w = winners b
  case length w of 1 -> putStrLn $ "The winner is:" ++ show w
                   _ -> putStrLn $ "The game is a tie between:" ++ show w

-- | The main loop for a human vs computer game
playVsComputer :: GameSetup -> GameTree -> IO ()
playVsComputer _ Exit = putStrLn "Thanks for playing"
playVsComputer g t@(GameTree _ _ _ []) = do
  printInfo g t
  announceWinner $ board t
playVsComputer g t = do
  printInfo g t
  case player t of 0 -> do
                     mv <- handleHuman t 
                     playVsComputer g mv
                   _ -> playVsComputer g $ handleHeuristicComputer g t

-- | The main loop for a computer player only game
cpuVsCpu :: GameSetup -> GameTree -> IO ()
cpuVsCpu _ Exit = putStrLn "Thanks for playing"
cpuVsCpu _ t@(GameTree _ _ _ []) = announceWinner $ board t
cpuVsCpu g t = cpuVsCpu g $ handleHeuristicComputer g t

-- | The main loop for a humans only game
playVsHuman :: GameSetup -> GameTree -> IO ()
playVsHuman _ Exit = putStrLn "Thanks for playing!"
playVsHuman g t = do
  printInfo g t
  case length (moves t) of 0 -> announceWinner $ board t
                           _ -> do 
                             brd <- handleHuman t
                             playVsHuman g brd

playerLabel :: Player -> String
playerLabel p = [chr (p + ord 'a')]

stringifyCell :: Cell -> String
stringifyCell (Cell p d) = printf "%s-%d" (playerLabel p) d

stringifyBoard :: GameSetup -> Vector Cell -> String
stringifyBoard g b = paddedStringifyBoard g b 0

paddedStringifyBoard :: GameSetup -> Vector Cell -> Int -> String
paddedStringifyBoard g b p = concat ls
  where s = boardSize g
        idxs = [ x * s | x <- [0..(s - 1)] ]
        ss = [ V.foldr (\c a -> stringifyCell c ++ " " ++ a) "\n" 
                $ V.slice x s b 
                | x  <- idxs ]
        p1s = [ concat (replicate (s - x) "  ") | x <- [0..(s-1)] ]
        p2s = [ concat (take p (cycle [" ","|"])) | _ <- [0..(s-1)] ]
        ps = zipWith (++) p2s p1s
        ls = zipWith (++) ps ss


stringifyTree :: GameSetup -> GameTree -> Int -> String
stringifyTree g t = strTree g t 0

-- | contains the logic to create a string of the board
strTree :: GameSetup -> GameTree -> Int -> Int -> String
strTree g t l d = concat (take (l*2) (cycle [" ","|"])) 
  ++ "Player:" ++ playerLabel (player t) ++ "\n" 
  ++ concat (take (l*2) (cycle [" ","|"])) ++ "attack:" 
  ++ show (attack t) ++ "\n"
  ++ concat (take (l*2) (cycle [" ","|"])) ++ "board:\n"
  ++ paddedStringifyBoard g (board t) (2*l) 
  ++ concat (take (l*2) (cycle [" ","|"])) 
  ++ "moves:\n" 
  ++ concat [strTree g m (l + 1) (d - 1) | m <- moves t]

-- | produces stringifies the options a player has for their current move
formatMoveOpt :: Int -> GameTree -> String
formatMoveOpt i (GameTree _ _ Nothing _) = printf "%d end turn" i
formatMoveOpt i (GameTree _ _ (Just (s,d)) _) = printf "%d  %d -> %d" i s d
formatMoveOpt _ Exit = "Exited"

-- | allows the human to select the move they want
handleHuman :: GameTree -> IO GameTree
handleHuman Exit = return Exit
handleHuman t = do
  putStrLn ""
  putStrLn "choose your move:"
  let moveCount = length $ moves t
      descs = [formatMoveOpt i (moves t !! i) | i <- [0..(moveCount-1)]]
  _ <- mapM putStrLn descs
  opt <- getLine
  case opt of "Q" -> return Exit
              "q" -> return Exit
              "quit" -> return Exit
              _ -> do
                let i = read opt :: Int
                return $ moves t !! i
