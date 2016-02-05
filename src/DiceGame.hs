module DiceGame where

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V
import Data.Char (ord, chr)
import Text.Printf
import System.Random
import Control.Monad.Random
import Data.MemoTrie

type Player = Int
data Cell = Cell {owner :: Player, dice :: Int} deriving (Show, Eq)
type Board = Vector Cell

type Attack = (Int, Int)

data GameSetup = GameSetup { playerCnt :: Int
                           , boardSize :: Int
                           , maxDice :: Int
                           , neighborF :: (Int -> [Int])
                           }-- deriving (Show)

data GameTree = GameTree { player :: Player
                         , board :: Board
                         , attack :: Maybe Attack
                         , moves :: [GameTree]
                         } 
                | Exit deriving (Show, Eq)

buildGS :: Int -> Int -> Int -> GameSetup
buildGS ps sz md = GameSetup ps sz md f
  where table = V.fromList [ neighbors sz x | x <- [0..(sz*sz-1)]]
        f = \x-> table ! x

type Rnd a = Rand StdGen a

dummyBoard :: Board
dummyBoard = V.fromList [Cell 0 3, Cell 0 3, Cell 1 3, Cell 1 3 ]
attackTestBoard :: Board
attackTestBoard = V.fromList [Cell 0 3, Cell 0 2, Cell 1 2, Cell 1 3 ]
attackTestBoard2 :: Board
attackTestBoard2 = V.fromList [Cell 0 3, Cell 1 2, Cell 1 2, Cell 1 1 ]
attackTestBoard3:: Board
attackTestBoard3 = V.fromList [Cell 0 3, Cell 0 3, Cell 1 3, Cell 1 1 ]

randBoard :: GameSetup -> Rnd (Vector Cell)
randBoard gs = V.replicateM l $ randCell p d
  where l = boardSize gs * boardSize gs
        p = playerCnt gs
        d = maxDice gs

generateBoard :: GameSetup -> IO (Vector Cell)
generateBoard gs = do
  evalRandIO $ randBoard gs

randCell :: Int -> Int -> Rnd Cell
randCell np d = do
  o <- getRandomR (0, (np - 1)) :: Rnd Int
  c <- getRandomR (1, d) :: Rnd Int
  return $ Cell o c

mGT = memo GameTree

buildTree :: GameSetup -> Board -> Player -> Int -> Bool -> Maybe Attack -> GameTree
buildTree g brd plyr srdc fm atk =
  mGT plyr brd atk $ addPassingMoves g brd plyr srdc fm $
    addAttackingMoves g brd plyr srdc

nxtPlyr :: Player -> GameSetup -> Player
nxtPlyr p g = rem (p + 1) $ playerCnt g

addPassingMoves :: GameSetup -> Board -> Player -> Int -> Bool -> [GameTree] -> [GameTree]
addPassingMoves _ _ _ _ True mvs = mvs
addPassingMoves g brd plyr srdc False mvs = 
   (buildTree g 
             (addNewDice g brd plyr (srdc - 1)) 
             (nxtPlyr plyr g) 
             0 
             True
             Nothing) : mvs

addNewDice :: GameSetup -> Board -> Player -> Int -> Board
addNewDice gs b p d = 
  case d > spots of True -> undefined
                    False -> b // updates
  where pcs = playerCells b p
        haveRoom = filter (\i-> diceAt i b < maxDice gs) pcs
        spots = length haveRoom
        updates = foldr (\i a-> (i, Cell p (diceAt i b + 1)):a) [] 
          $ take d haveRoom

playerAt :: Int -> Board -> Player
playerAt pos brd = owner $ brd ! pos

diceAt :: Int -> Board -> Int
diceAt pos brd = dice $ brd ! pos

ownedByP :: Player -> Cell -> Bool
ownedByP p c = p == owner c

neighbors :: Int -> Int -> [Int]
neighbors sz pos = filter (>= 0) . filter (< ml) $ concat [g2, g3]
  where ml = sz * sz
        up = pos - sz
        down = pos + sz
        leftEdgeP = case pos `rem` sz of 0 -> True
                                         _ -> False
        rightEdgeP = case (pos + 1) `rem` sz of 0 -> True
                                                _ -> False
        g2 = case leftEdgeP of False -> [up -1, up, pos -1]
                               True -> [up]
        g3 = case rightEdgeP of False -> [pos + 1, down, down + 1]
                                True -> [down]

playerCells:: Board -> Player -> [Int]
playerCells b p = V.toList $ V.findIndices (ownedByP p) b

removeFriendlies :: Player -> Board -> [Int] -> [Int]
removeFriendlies p b = filter (\x -> not (ownedByP p (b ! x)))

potentialTargets :: Player -> Board -> [Int] -> [[Int]] -> [(Int, [Int])]
potentialTargets p b srcs ns = zip srcs enemies
  where enemies = map (removeFriendlies p b) ns

winnable :: Board -> [(Int, [Int])] -> [(Int, [Int])]
winnable b = map (\(s, ds)->(s, filter (\d-> diceAt d b < diceAt s b) ds))

attacks :: GameSetup -> Board -> Player -> [ (Int, Int) ]
attacks g b p = concat $ map (\(s,ds)-> [(s, d)| d <- ds]) val
  where srcs = playerCells b p
        nes = map (removeFriendlies p b . neighborF g) srcs
        val = winnable b $ zip srcs nes

addAttackingMoves :: GameSetup -> Board -> Player -> Int -> [GameTree]
addAttackingMoves g b p sprd = gts
  where ats = attacks g b p
        dice1 = map (\(s, _)-> diceAt s b) ats
        dice2 = map (\(_, d)-> diceAt d b) ats
        zd = zip3 ats dice1 dice2
        gts = [
          buildTree g (attackBoard b p a ds) p (sprd + dd) False $ Just a | 
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
formatMoveOpt i (GameTree _ _ Nothing _) = printf "%d end turn" i
formatMoveOpt i (GameTree _ _ (Just (s,d)) _) = printf "%d  %d -> %d" i s d
formatMoveOpt _ Exit = "Exited"

handleHuman :: GameTree -> IO GameTree
handleHuman Exit = do
  return Exit
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
playVsHuman _ Exit = do
  putStrLn "Thanks for playing!"
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
        p1s = [ concat $ (take (s - x) $ repeat "  ") | x <- [0..(s-1)] ]
        p2s = [ concat $ (take (p) (cycle [" ","|"])) | _ <- [0..(s-1)] ]
        ps = zipWith (++) p2s p1s
        ls = zipWith (++) ps ss


stringifyTree :: GameSetup -> GameTree -> Int -> String
stringifyTree g t d = strTree g t 0 d

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

getRatings :: GameTree -> Player -> [Float]
getRatings t p = map (\m-> ratePosition m p) $ moves t

ratePosition :: GameTree -> Player -> Float
ratePosition Exit _ = 0.0
ratePosition (GameTree _ b _ []) p = if p `elem` w 
                                     then 1.0 / (fromIntegral $ length w)
                                     else 0.0
  where w = winners b
ratePosition t@(GameTree cp _ _ _) p = f ( getRatings t p)
  where f = case p == cp of True -> (maximum) 
                            False -> (minimum)

handleComputer :: GameTree -> GameTree
handleComputer t@(GameTree p _ _ ms) = head $ 
    dropWhile (\m->ratePosition m p < mx ) ms
  where rs = getRatings t $ player t
        mx = maximum rs
handleComputer Exit = Exit

playVsComputer :: GameSetup -> GameTree -> IO ()
playVsComputer _ Exit = do
  putStrLn "Thanks for playing"
playVsComputer g t@(GameTree _ _ _ []) = do
  printInfo g t
  announceWinner $ board t
playVsComputer g t = do
  printInfo g t
  case player t of 0 -> do
                     mv <- handleHuman t 
                     playVsComputer g mv
                   _ -> playVsComputer g $ handleComputer t

cpuVsCpu :: GameSetup -> GameTree -> IO ()
cpuVsCpu _ Exit = do
  putStrLn "Thanks for playing"
cpuVsCpu g t@(GameTree _ _ _ []) = do
  announceWinner $ board t
cpuVsCpu g t = do
  cpuVsCpu g $ handleComputer t
