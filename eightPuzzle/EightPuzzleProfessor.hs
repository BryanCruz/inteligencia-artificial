import Data.List

-- | data types for the puzzle
type State = [[Int]]
data Move  = Up | Down | Lft | Rgt 
             deriving (Show, Read, Enum, Eq, Ord)

type Thread = [Move]

-- | utility functions
empty = null

argmin = (snd.head.sort)

addTuple :: Num a => (a, a) -> (a, a) -> (a, a)
addTuple (x1, x2) (y1, y2) = (x1+y1, x2+y2)

absDiff (x1,y1) (x2,y2) = d1 + d2
  where d1 = abs (x1 - x2)
        d2 = abs (y1 - y2)

dist :: Int -> State -> State -> Int
dist v st1 st2 = absDiff (findNum v st1) (findNum v st2)
       
findNum :: Int -> State -> (Int, Int)
findNum val s = head [ (x, y) | x <- [0..2], y <- [0..2], 
                                (s !! x) !! y == val ]

findZero :: State -> (Int, Int)
findZero = findNum 0

inBound :: Int -> Bool
inBound x | x >= 0 && x <= 2 = True
          | otherwise        = False

-- | Swap two values of a given state
swap :: Int -> State -> State
swap val s = map (map change) s
  where change sij | sij == val = 0 
                   | sij == 0   = val
                   | otherwise  = sij

-- | Maps a Move to a coordinate
pos :: Move -> (Int, Int)
pos Up   = (-1,  0)
pos Down = ( 1,  0)
pos Lft  = ( 0, -1)
pos Rgt  = ( 0,  1)

-- | Update state
move :: Move -> (State, Thread) -> (State, Thread)
move dir (s, t) = (s', dir : t)
  where 
    s'  = swap val s
    val | inBound newX && inBound newY = (s !! newX) !! newY
        | otherwise                    = 0         
    (newX, newY) = addTuple (findZero s) (pos dir)

-- | Initial state
s0 :: State
s0 = [[1, 8, 2], [0, 4, 3], [7, 6, 5]]

-- | Goal state
goalSt = [[1,2,3],[4,5,6],[7,8,0]]

-- | Check if it is a goal state
isGoal :: (State, Thread) -> Bool
isGoal (s, _) = s == goalSt

-- | For this puzzle, every state is feasible
feasible :: a -> Bool
feasible x = True

-- | perform a move
choices = [Up .. Rgt] 
moves   = pure move <*> choices

-- | Breadth-first search
bfs :: [(State, Thread)] -> (State, Thread)
bfs []                     = error "Couldn't find a feasible solution"
bfs sts | (not.empty) goal = head goal
        | otherwise        = bfs sts'
        where goal = filter isGoal sts'
              sts' = filter feasible $ moves <*> sts

-- | A* search
h :: (State, Thread) -> Int
h (s, t) = length t + sum [dist v s goalSt | v <- [0..8]] -- h = g + f

astar :: [(State, Thread)] -> (State, Thread)
astar []                     = error "Couldn't find a feasible solution"
astar sts | (not.empty) goal = head goal
          | otherwise        = astar sts'
          where goal  = filter isGoal sts'
                sts'  = filter (/=s') $ sts ++ (moves <*> [s'])
                s'    = argmin $ zip (map h sts) sts                

main = do 
  print (bfs [(s0,[])])
  print (astar [(s0,[])])
