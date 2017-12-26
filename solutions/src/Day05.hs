module Day05 (day05) where

data Maze = Maze { step :: Int
                 , current :: Int
                 , jumps :: [Int]
                 } deriving (Show)

day05 :: IO ()
day05 = do
    content <- readFile "input5.txt"
    (print . step) $ until hasExit jump (readInput content)

readInput :: String -> Maze
readInput content = Maze {step=0, current=0, jumps= (map readF . lines) content }
    where readF = read :: String -> Int

hasExit :: Maze -> Bool
hasExit maze = current maze >= length (jumps maze)

jump :: Maze -> Maze
jump maze = if hasExit maze then maze else Maze {step = step maze + 1, current=newCurrent, jumps=newJumps}
    where index = current maze
          move = jumps maze !! index
          newCurrent = current maze + move
          newJumps = take index (jumps maze) ++ [move + 1] ++ drop (index + 1) (jumps maze)
