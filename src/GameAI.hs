module GameAI where

import DiceGame

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

handleComputer :: GameSetup -> GameTree -> GameTree
handleComputer g t@(GameTree p _ _ ms) = head $ 
    dropWhile (\m->ratePosition m p < mx ) ms
  where mvs = limitTreeDepth t $ aiLevel g
        rs = getRatings mvs $ player t
        mx = maximum rs
handleComputer _ Exit = Exit


-- | returns a GameTree that is restricted to the given depth of positions
limitTreeDepth _ 0 = Exit
limitTreeDepth Exit _ = Exit
limitTreeDepth (GameTree p b a _ms) 1 =
  mGT p b a []
limitTreeDepth t@(GameTree p b a ms) d = 
  mGT p b a $ map (\m->limitTreeDepth m (d - 1)) ms

-- | returns a GameTree that is restricted to the given depth of turns
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
