module Day05 where

import qualified Data.Sequence as Seq

-- performance using bulit-in list
-- real  9m33.902s
-- user 14m33.891s
-- sys   4m13.832s

-- performance using Seq
-- real 0m16.577s
-- user 0m20.852s
-- sys  0m23.677s
data Maze = Maze { step :: Int
                 , current :: Int
                 , jumps :: Seq.Seq Int
                 } deriving (Show)

day05 :: IO ()
day05 = do
    content <- readFile "input5.txt"
    -- putStrLn "Part I"
    -- (print . step) $ until hasExit jump (readInput content)
    putStrLn "Part II"
    (print . step) $ until hasExit jumpII (readInput content)


readInput :: String -> Maze
readInput content = Maze {step=0, current=0, jumps= (Seq.fromList . map readF . lines) content }
    where readF = read :: String -> Int

hasExit :: Maze -> Bool
hasExit maze = current maze >= Seq.length (jumps maze)

jump :: Maze -> Maze
jump maze = if hasExit maze then maze else Maze {step = step maze + 1, current=newCurrent, jumps=newJumps}
    where index = current maze
          move = Seq.index (jumps maze) index
          newCurrent = current maze + move
          newJumps = Seq.update index (move + 1) (jumps maze)

jumpII :: Maze -> Maze
jumpII maze = if hasExit maze then maze else Maze {step = step maze + 1, current=newCurrent, jumps=newJumps}
    where index = current maze
          move = Seq.index (jumps maze) index
          newCurrent = current maze + move
          newJumps = Seq.update index (f move) (jumps maze)
          f x = if (x >= 3) then x - 1 else x + 1


