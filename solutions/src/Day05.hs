module Day05 where

data Maze = Maze { step :: Int
                 , current :: Int
                 , jumps :: [Int]
                 } deriving (Show)

day05 :: IO ()
day05 = undefined

hasExit :: Maze -> Bool
hasExit maze = current maze >= length (jumps maze)

jump :: Maze -> Maze
jump maze = if hasExit maze then maze else Maze {step = step maze + 1, current=newCurrent, jumps=newJumps}
    where index = current maze
          move = jumps maze !! index
          newCurrent = current maze + move
          newJumps = take index (jumps maze) ++ [move + 1] ++ drop (index + 1) (jumps maze)
