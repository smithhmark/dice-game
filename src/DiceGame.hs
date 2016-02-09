module DiceGame where

import qualified Data.Map.Strict as M
import Data.Vector (Vector, (!), (//))
import Data.List
import qualified Data.Vector as V
import System.Random
import Control.Monad.Random
import Control.Monad.Reader
import Data.MemoTrie

-- | Players are really Ints, but makes the type sigs more helpful
type Player = Int

data Cell = Cell { owner :: Player
                 , dice :: Int
                 } deriving (Show, Eq)
type Board = Vector Cell

-- | the Attack type represents one player attacking another
-- the first number is the Vector index of the cell launching the attack
-- the second is the Vector index of the cell being attacked
type Attack = (Int, Int)

data GameSetup = GameSetup { playerCnt :: Int
                           , boardSize :: Int
                           , maxDice :: Int
                           , aiLevel :: Int
                           , neighborF :: (Int -> [Int])
                           }-- deriving (Show)

data GameTree = GameTree { player :: Player
                         , board :: Board
                         , attack :: Maybe Attack
                         , moves :: [GameTree]
                         } 
                | Exit deriving (Show, Eq)

buildGS :: Int -> Int -> Int -> GameSetup
buildGS ps sz md = GameSetup ps sz md 2 f
  where table = V.fromList [ neighbors sz x | x <- [0..(sz*sz-1)]]
        f = \x-> table ! x

-- | This type makes generating random boards slightly cleaner
type Rnd a = Rand StdGen a

dummyBoard :: Board
dummyBoard = V.fromList [Cell 0 3, Cell 0 3, Cell 1 3, Cell 1 3 ]
attackTestBoard :: Board
attackTestBoard = V.fromList [Cell 0 3, Cell 0 2, Cell 1 2, Cell 1 3 ]
attackTestBoard2 :: Board
attackTestBoard2 = V.fromList [Cell 0 3, Cell 1 2, Cell 1 2, Cell 1 1 ]
attackTestBoard3:: Board
attackTestBoard3 = V.fromList [Cell 0 3, Cell 0 3, Cell 1 3, Cell 1 1 ]

-- | Generates a random board from a GameSetup
randBoard :: GameSetup -> Rnd (Vector Cell)
randBoard gs = V.replicateM l $ randCell p d
  where l = boardSize gs * boardSize gs
        p = playerCnt gs
        d = maxDice gs

-- | public function to generate a random board from a GameSetup
generateBoard :: GameSetup -> IO (Vector Cell)
generateBoard gs = do
  evalRandIO $ randBoard gs

-- | knowns enough of the game rules to bulid a cell from 
-- the number of players and he max number of dice
randCell :: Int -> Int -> Rnd Cell
randCell np d = do
  o <- getRandomR (0, (np - 1)) :: Rnd Int
  c <- getRandomR (1, d) :: Rnd Int
  return $ Cell o c

-- | using a MemoTrie to speed up the creation of GameTrees
mGT = memo GameTree

-- | bulids the tree of all legal moves rooted at a given board position
buildTree :: Board  -- ^ the starting position for the tree
          -> Player  -- ^ the player to move from this position
          -> Int  -- ^ how many dice have been caputured this turn
          -> Bool  -- ^ if this is the first move of the player's turn
          -> Maybe Attack  -- ^ an Attack if that is how we got to the board, Nothing otherwise
          -> Reader GameSetup GameTree  -- ^ the root node of the GameTree
buildTree brd plyr srdc fm atk = do
  g <- ask
  return $ mGT plyr brd atk $ addPassingMoves g brd plyr srdc fm $
    addAttackingMoves g brd plyr srdc

-- | selects the next player based on the current player
nxtPlyr :: Player -> GameSetup -> Player
nxtPlyr p g = rem (p + 1) $ playerCnt g

-- | helper function to buildTree that adds the turn-end move
addPassingMoves :: GameSetup -- ^ the game configuration
                -> Board  -- ^ the board the player is passing on
                -> Player  -- ^ the player doing the passing
                -> Int  -- ^ how many dice the player captured prior to passing
                -> Bool  -- ^ if this is the player's first move of the turn
                -> [GameTree]  -- ^ the moves that an attack would generate
                -> [GameTree]  -- ^ the attack moves plus the possible passing move
addPassingMoves _ _ _ _ True mvs = mvs
addPassingMoves g brd plyr srdc False mvs = 
   (runReader (buildTree (addNewDice g brd plyr (srdc - 1)) 
              (nxtPlyr plyr g) 
              0 
              True
              Nothing) g) : mvs

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
playerAt pos brd = owner $! brd ! pos

diceAt :: Int -> Board -> Int
diceAt pos brd = dice $! brd ! pos

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
playerCells b p = V.toList $! V.findIndices (ownedByP p) b

removeFriendlies :: Player -> Board -> [Int] -> [Int]
removeFriendlies p b = filter pred
  where pred x = not (ownedByP p (b ! x))

removeFriendlies2 :: [Int] -> [Int] -> [Int]
removeFriendlies2 ps qs = qs \\ ps

potentialTargets :: Player -> Board -> [Int] -> [[Int]] -> [(Int, [Int])]
potentialTargets p b srcs ns = zip srcs enemies
  where enemies = map (removeFriendlies p b) ns

-- | given a [Board] and a list of attack possibilities, filter them to only
-- those with a favorable dice imbalance.
winnable :: Board -> [(Int, [Int])] -> [(Int, [Int])]
winnable b = map (\(s, ds)->(s, filter (\d-> diceAt d b < diceAt s b) ds))

-- | produces the list of viable attack moves
attacks :: GameSetup -> Board -> Player -> [ (Int, Int) ]
attacks g b p = concat $ map (\(s,ds)-> [(s, d)| d <- ds]) val
  where srcs = playerCells b p
        nes = map (removeFriendlies2 srcs . neighborF g) srcs
        val = winnable b $ zip srcs nes

-- | produces the list of viable post-attack board positions
addAttackingMoves :: GameSetup -> Board -> Player -> Int -> [GameTree]
addAttackingMoves g b p sprd = gts
  where ats = attacks g b p
        dice1 = map (\(s, _)-> diceAt s b) ats
        dice2 = map (\(_, d)-> diceAt d b) ats
        zd = zip3 ats dice1 dice2
        gts = [
          runReader (buildTree (attackBoard b p a ds) p (sprd + dd) False $ Just a ) g | 
               (a, ds, dd) <- zd]

attackBoard :: Board -> Player -> (Int, Int) -> Int -> Board
attackBoard b p (src,dst) d = b // [sc, dc]
  where sc = (src, Cell p 1)
        dc = (dst, Cell p (d - 1))

territoryCount :: Board -> M.Map Player Int
territoryCount brd = foldr work M.empty brd
  where work (Cell p _d) ac= M.insertWith (+) p 1 ac

winners :: Board -> [Player]
winners b = M.keys $ M.filter (== mx) cnts
  where cnts = territoryCount b
        mx = maximum $ M.elems cnts

