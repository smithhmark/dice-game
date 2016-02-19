module GameAI where

import Data.Vector (Vector, (!), (//))
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.PQueue.Prio.Max as MxQ
import qualified Data.PQueue.Prio.Min as MnQ
import qualified Safe as S

import DiceGame

getRatings :: GameTree -> Player -> [Float]
getRatings = getRatingsA

getRatingsA :: GameTree -> Player -> [Float]
getRatingsA t p = map (`ratePosAbsolute` p) $ moves t

ratePosition :: GameTree -> Player -> Float
ratePosition = ratePosAbsolute

-- | Rate position using minimax over the whole game tree
ratePosAbsolute :: GameTree -- ^ the game tree we are scoring
                -> Player -- ^ the Player that the tree is being scored for
                -> Float -- ^ the score being returned
ratePosAbsolute Exit _ = 0.0 -- ^ a degenerate case that shouldn't happen
ratePosAbsolute (GameTree _ b _ []) p = if p `elem` w 
                                        then 1.0 / fromIntegral (length w)
                                        else 0.0
  where w = winners b
ratePosAbsolute t@(GameTree cp _ _ _) p = f ( getRatingsA t p)
  where f = if p == cp then maximum else minimum

-- | a function to select the move the computer will make
handlePerfectComputer :: GameTree -- ^ the current game position
                      -> GameTree -- ^ the resulting game position
handlePerfectComputer t@(GameTree p _ _ ms) = snd $ MxQ.findMax q
  where ifn a (v, k) = MxQ.insert k v a
        q = L.foldl' ifn MxQ.empty $ zip ms $ getRatingsA t $ player t
handlePerfectComputer Exit = Exit


-- | makes of copy of a [GameTree] to a limited depth
limitTreeDepth :: GameTree -- ^ The tree to limit
               -> Int -- ^ how many children positions to include
               -> GameTree -- ^ the limited tree
limitTreeDepth _ 0 = Exit
limitTreeDepth Exit _ = Exit
limitTreeDepth (GameTree p b a _ms) 1 =
  mGT p b a []
limitTreeDepth t@(GameTree p b a ms) d = 
  mGT p b a $ map (\m->limitTreeDepth m (d - 1)) ms

-- | makes of copy of a [GameTree] to a limited depth based on players' turns
limitTreeDepth' :: GameTree -- ^ The tree to limit
                -> Int -- ^ how many turns of play to include
                -> GameTree -- ^ the limited tree
limitTreeDepth' Exit _ = Exit
limitTreeDepth' (GameTree p b a ms) 0 = 
  mGT p b a $ concatMap (\st@(GameTree lp _ _ ms) ->
      if lp == p 
      then [limitTreeDepth' st 0]
      else []) ms
limitTreeDepth' (GameTree p b a ms) d = 
  mGT p b a $ map (
    \st@(GameTree lp _ _ ms) ->
      if lp == p 
      then limitTreeDepth' st d
      else limitTreeDepth' st (d - 1)) ms

-- | determines if a cell at the index into the board is threatened.
-- to be threatened means that the cell has an enemy neighbor with more dice
threatened :: GameSetup 
           -> Int  --- ^ the index into the board
           -> Board --- ^ the board
           -> Bool
threatened gs pos b = 
  let Cell player dice = b ! pos
      ns = neighborF gs pos
      es = removeFriendlies player b ns
      pred i 
        | diceAt i b > dice = True
        | otherwise = False
  in all pred es

-- | scores a board. Threatened cells are worth one, unthreated cells 2.
scoreBoard :: GameSetup -> Board -> Player -> Int
scoreBoard gs brd p = sum . V.toList $ V.imap sfn brd
  where sfn i (Cell p2 _d) = if p2 == p
                             then if threatened gs i brd then 1 else 2
                             else -1

getRatingsH :: GameSetup -> GameTree -> Player -> [Float]
getRatingsH gs t p = map (\m-> ratePosHeuristic gs m p) $ moves t

-- | use a heuristic to rate the player's given position
ratePosHeuristic :: GameSetup -> GameTree -> Player -> Float
ratePosHeuristic _ Exit _ = 0.0
ratePosHeuristic gs (GameTree _ b _ []) p = fromIntegral $ scoreBoard gs b p
ratePosHeuristic gs t@(GameTree cp _ _ _) p = f ( getRatingsH gs t p)
  where f = if p == cp then maximum else minimum

rateOwnChildren :: GameSetup 
                -> GameTree
                -> Player
                -> MxQ.MaxPQueue Float GameTree
rateOwnChildren _gs Exit _p = MxQ.empty -- ^ degenerate case
rateOwnChildren _gs (GameTree _ _ _ []) _p = MxQ.empty 
rateOwnChildren gs t@(GameTree _p _b _a ms) p = L.foldl' work MxQ.empty ms
  where work acc m = let score = fst $ ratePosHeuristic2 gs m p
                     in MxQ.insert score m acc

rateOtherChildren :: GameSetup 
                  -> GameTree
                  -> Player
                  -> MnQ.MinPQueue Float GameTree
rateOtherChildren _gs Exit _p = MnQ.empty
rateOtherChildren _gs (GameTree _ _ _ []) _p = MnQ.empty
rateOtherChildren gs t@(GameTree _p _b _a ms) p = L.foldl' work MnQ.empty ms
  where work acc m = let score = fst $ ratePosHeuristic2 gs m p
                     in MnQ.insert score m acc

-- | use a heuristic to rate the player's given position
ratePosHeuristic2 :: GameSetup -> GameTree -> Player -> (Float, GameTree)
ratePosHeuristic2 _ Exit _ = (0.0, Exit)
ratePosHeuristic2 gs t@(GameTree _ b _ []) p = (
  fromIntegral $ scoreBoard gs b p, t)
ratePosHeuristic2 gs t@(GameTree cp _ _ _) p = if p == cp 
  then MxQ.findMax $ rateOwnChildren gs t p
  else MnQ.findMin $ rateOtherChildren gs t p

-- | a function to select the move the computer will make
handleHeuristicComputer :: GameSetup -- ^ the config
                        -> GameTree -- ^ the current game position
                        -> GameTree -- ^ the resulting game position
handleHeuristicComputer _ Exit = Exit
handleHeuristicComputer gs t@(GameTree p _ _ ms) = 
  snd $ ratePosHeuristic2 gs t p 



