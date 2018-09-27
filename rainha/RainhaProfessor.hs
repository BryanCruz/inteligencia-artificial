import Control.Monad

-- | data types for the puzzle
type Row    = Int
type State  = [Row]
type Thread = [Row]

-- | utility functions
empty = null
boardSize = 30

-- | Check for infeasible states
infeasible :: (State, Thread) -> Bool
infeasible ([], _)    = False
infeasible ((r:rs),t) = length rs > (boardSize-1) || attack r rs || infeasible (rs, t)

feasible = (not.infeasible)

-- | Check if a row is attacking another row of a state
attack :: Row -> [Row] -> Bool
attack r rs = r `elem` rs
            || r `elem` (upperDiag rs)
            || r `elem` (lowerDiag rs)
  where 
    upperDiag xs = zipWith (-) xs [1..]
    lowerDiag xs = zipWith (+) xs [1..]

-- | Check if it is a goal state
isGoal :: (State, Thread) -> Bool
isGoal (rs,t) = (feasible (rs,t)) && (length rs == boardSize)

-- | Perform a move
move :: Int -> (State, Thread) -> (State, Thread)
move x (s,t)  = (x:s, x:t)

choices = [1..boardSize]
moves   = pure move <*> choices

emptySt = ([],[])

-- | Breadth-first search
bfs :: [(State, Thread)] -> (State, Thread)
bfs []                     = error "Couldn't find a feasible solution"
bfs sts | (not.empty) goal = head goal
        | otherwise        = bfs sts'
  where 
    goal = filter isGoal sts'
    sts' = filter feasible $ moves <*> sts

-- | Depth-first search
dfs :: (State, Thread) -> [(State, Thread)]
dfs st | isGoal st     = [st]
       | infeasible st = [emptySt]
       | otherwise    = do x   <- [1..boardSize]
                           st' <- dfs $ move x st
                           guard $ st' /= emptySt
                           return st'

main = do
  print (head $ dfs emptySt)
  --print (bfs [emptySt])
