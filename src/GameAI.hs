module GameAI where

import Data.Vector (Vector, (!), (//))
import qualified Data.Vector as V

import DiceGame

getRatings :: GameTree -> Player -> [Float]
getRatings = getRatingsA

getRatingsA :: GameTree -> Player -> [Float]
getRatingsA t p = map (\m-> ratePosAbsolute m p) $ moves t

ratePosition :: GameTree -> Player -> Float
ratePosition = ratePosAbsolute

-- | Rate position using minimax over the whole game tree
ratePosAbsolute :: GameTree -- ^ the game tree we are scoring
                -> Player -- ^ the Player that the tree is being scored for
                -> Float -- ^ the score being returned
ratePosAbsolute Exit _ = 0.0 -- ^ a degenerate case that shouldn't happen
ratePosAbsolute (GameTree _ b _ []) p = if p `elem` w 
                                        then 1.0 / (fromIntegral $ length w)
                                        else 0.0
  where w = winners b
ratePosAbsolute t@(GameTree cp _ _ _) p = f ( getRatingsA t p)
  where f = case p == cp of True -> (maximum) 
                            False -> (minimum)

-- | a function to select the move the computer will make
handlePerfectComputer :: GameSetup -- ^ the config
                      -> GameTree -- ^ the current game position
                      -> GameTree -- ^ the resulting game position
handlePerfectComputer g t@(GameTree p _ _ ms) = head $ 
    dropWhile (\m->ratePosAbsolute m p < mx ) ms
  where rs = getRatingsA t $ player t
        mx = maximum rs
handlePerfectComputer _ Exit = Exit


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
  mGT p b a $ concat $ map (\st@(GameTree lp _ _ ms) ->
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
  in and $ map pred es

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
  where f = case p == cp of True -> (maximum) 
                            False -> (minimum)

-- | a function to select the move the computer will make
handleHeuristicComputer :: GameSetup -- ^ the config
                        -> GameTree -- ^ the current game position
                        -> GameTree -- ^ the resulting game position
handleHeuristicComputer gs t@(GameTree p _ _ ms) = head $ 
    dropWhile (\m->ratePosHeuristic gs  m p <  mx - 0.0001 ) ms
  where mvs = limitTreeDepth' t $ aiLevel gs
        rs = getRatingsH gs mvs $ player t
        mx = maximum rs
handleHeuristicComputer _ Exit = Exit
