module DiceGame where

import Data.Vector (Vector, cons, (!), (!?), (//))
import qualified Data.Vector as V
import Data.Char (ord, chr)
import Text.Printf

type Player = Int
data Cell = Cell {owner :: Player, dice :: Int} deriving (Show, Eq)
type Board = Vector Cell

type Attack = (Int, Int)

data GameSetup = GameSetup { playerCnt :: Int
                           , boardSize :: Int
                           , maxDice :: Int
                           } deriving (Show)

data GameTree = GameTree { player :: Player
                         , board :: Board
                         , attack :: Maybe Attack
                         , moves :: [GameTree]
                         } deriving (Show, Eq)

dummyBoard = V.fromList [Cell 0 3, Cell 0 3, Cell 1 3, Cell 1 3 ]
attackTestBoard = V.fromList [Cell 0 3, Cell 0 2, Cell 1 2, Cell 1 3 ]
attackTestBoard2 = V.fromList [Cell 0 3, Cell 1 2, Cell 1 2, Cell 1 1 ]
attackTestBoard3 = V.fromList [Cell 0 3, Cell 0 3, Cell 1 3, Cell 1 1 ]

buildTree :: GameSetup -> Board -> Player -> Int -> Bool -> Maybe Attack -> GameTree
buildTree g brd plyr srdc fm atk =
  GameTree plyr brd atk $ addPassingMoves g brd plyr srdc fm $ addAttackingMoves g brd plyr srdc

nxtPlyr :: Player -> GameSetup -> Player
nxtPlyr p g = mod (p + 1) $ playerCnt g

addPassingMoves :: GameSetup -> Board -> Player -> Int -> Bool -> [GameTree] -> [GameTree]
addPassingMoves g brd plyr srdc True mvs = mvs
addPassingMoves g brd plyr srdc False mvs = 
   (buildTree g 
             (addNewDice g brd plyr (srdc - 1)) 
             (nxtPlyr plyr g) 
             0 
             True
             Nothing) : mvs

addNewDice :: GameSetup -> Board -> Player -> Int -> Board
addNewDice g b p d = 
  case d > spots of True -> undefined
                    False -> b // updates
  where pcs = playerCells g b p
        haveRoom = filter (\i-> maxDice g > diceAt i b) pcs
        spots = length haveRoom
        uB d (i:is) = (i, Cell p (diceAt i b + 1)):uB (d-1) is
        uB 0 _ = []
        uB _ [] = []
        updates = uB d haveRoom

playerAt :: Int -> Board -> Player
playerAt pos brd = owner $ brd ! pos

diceAt :: Int -> Board -> Int
diceAt pos brd = dice $ brd ! pos

ownedByP :: Player -> Cell -> Bool
ownedByP p c = p == owner c

neighbors :: GameSetup -> Board -> Int -> [Int]
neighbors g brd pos = filter (>= 0) . filter (< V.length brd) $ concat [g2, g3]
  where sz = boardSize g
        up = pos - sz
        down = pos + sz
        leftEdgeP = case pos `mod` sz of 0 -> True
                                         _ -> False
        rightEdgeP = case (pos + 1) `mod` sz of 0 -> True
                                                _ -> False
        g2 = case leftEdgeP of False -> [up -1, up, pos -1]
                               True -> [up]
        g3 = case rightEdgeP of False -> [pos + 1, down, down + 1]
                                True -> [down]

playerCells:: GameSetup -> Board -> Player -> [Int]
playerCells g b p = V.toList $ V.findIndices (ownedByP p) b

potentialTargets :: Player -> Board -> [Int] -> [[Int]] -> [(Int, [Int])]
potentialTargets p b srcs ns = zip srcs enemies
  where enemies = map (filter (\x-> not (ownedByP p (b ! x)))) ns

winnable :: Player -> Board -> [(Int, [Int])] -> [(Int, [Int])]
winnable p b pts = map (\(s, ds)->(s, filter (\d-> diceAt d b < diceAt s b) ds)) pts 

attacks :: GameSetup -> Board -> Player -> [ (Int, Int) ]
attacks g b p = concat $ map (\(s,ds)-> [(s, d)| d <- ds]) val
  where srcs = playerCells g b p
        ns = map (neighbors g b) srcs
        pts = potentialTargets p b srcs ns
        val = winnable p b pts 

addAttackingMoves :: GameSetup -> Board -> Player -> Int -> [GameTree]
addAttackingMoves g b p sprd = gts
  where ats = attacks g b p
        dice1 = map (\(s, _)-> diceAt s b) ats
        dice2 = map (\(_, d)-> diceAt d b) ats
        zd = zip3 ats dice1 dice2
        gts = [buildTree g (attackBoard b p a ds) p (sprd + dd) False $ Just a | 
               (a, ds, dd) <- zd]

attackBoard :: Board -> Player -> (Int, Int) -> Int -> Board
attackBoard b p (src,dst) d = b // [sc, dc]
  where sc = (src, Cell p 1)
        dc = (dst, Cell p (d - 1))

printInfo :: GameSetup -> GameTree -> IO ()
printInfo g t = do
  putStrLn ""
  putStrLn $ printf "current player = %s" $ playerLabel $ player t
  putStr . stringifyBoard g $ board t

announceWinner :: Board -> IO ()
announceWinner b = do
  putStrLn ""
  let w = winners b
  --case length w of 1 -> putStrLn $ "The winner is:" ++ playerLabel $ head w
  case length w of 1 -> putStrLn $ "The winner is:" ++ show w
                   _ -> putStrLn $ "The game is a tie between:" ++ show w

formatMoveOpt :: Int -> GameTree -> String
formatMoveOpt i (GameTree p b Nothing m) = printf "%d end turn" i
formatMoveOpt i (GameTree p b (Just (s,d)) m) = printf "%d  %d -> %d" i s d

handleHuman :: GameTree -> IO GameTree
handleHuman t = do
  putStrLn ""
  putStrLn "choose your move:"
  let moveCount = length $ moves t
      descs = [formatMoveOpt i (moves t !! i) | i <- [0..(moveCount-1)]]
  mapM putStrLn descs
  opt <- getLine
  let i = read opt :: Int
  return $ (moves t) !! i

playerCounts :: Board -> [(Int, Player)] -> [(Int, Player)]
playerCounts b ws = case lo of 0 -> (lp, p):ws
                               _ -> playerCounts ocs $ (lp, p):ws
  where p = playerAt 0 b
        (pcs, ocs) = V.partition (\c-> p == owner c) b
        lp = V.length pcs
        lo = V.length ocs

winners :: Board -> [Player]
winners b = map snd $ filter (\c-> fst c == mx) cnts
  where cnts = playerCounts b []
        mx = foldl (\a (c, _p)-> if c > a then c else a) 0 cnts

playVsHuman :: GameSetup -> GameTree -> IO ()
playVsHuman g t = do
  printInfo g t
  case length (moves t) of 0 -> announceWinner $ board t
                           _ -> do 
                             board <- handleHuman t
                             playVsHuman g board

playerLabel p = [chr (p + ord 'a')]

stringifyCell :: Cell -> String
stringifyCell (Cell p d) = printf "%s-%d" (playerLabel p) d

stringifyBoard :: GameSetup -> Vector Cell -> String
stringifyBoard g b = paddedStringifyBoard g b 0

paddedStringifyBoard :: GameSetup -> Vector Cell -> Int -> String
paddedStringifyBoard g b p = concat ls
  where s = boardSize g
        idxs = [ x * s | x <- [0..(s - 1)] ]
        ss = [ V.foldr (\c a -> stringifyCell c ++ " " ++ a) "\n" $ V.slice x s b | x  <- idxs ]
        p1s = [ concat $ (take (s - x) $ repeat "  ") | x <- [0..(s - 1)] ]
        p2s = [ concat $ (take (p) (cycle [" ","|"])) | x <- [0..(s-1)] ]
        ps = zipWith (++) p2s p1s
        ls = zipWith (++) ps ss

stringifyTree g t d = strTree g t 0 d

strTree g t l d = concat (take (l*2) (cycle [" ","|"])) 
  ++ "Player:" ++ playerLabel (player t) ++ "\n" 
  ++ concat (take (l*2) (cycle [" ","|"])) ++ "attack:" 
  ++ show (attack t) ++ "\n"
  ++ concat (take (l*2) (cycle [" ","|"])) ++ "board:\n"
  ++ paddedStringifyBoard g (board t) (2*l) 
  ++ concat (take (l*2) (cycle [" ","|"])) 
  ++ "moves:\n" 
  ++ concat [strTree g m (l + 1) (d - 1) | m <- moves t]
