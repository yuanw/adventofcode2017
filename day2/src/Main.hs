module Main where


checkRow :: [Int] -> Int
checkRow elems = maxElement - minElement
  where minElement = minimum elems
        maxElement = maximum elems

checkSum :: [[Int]] -> Int
checkSum = sum . map checkRow        

readRows :: String -> [[Int]]
readRows = map f . lines
   where f = (map read) . words :: String -> [Int] 

checkRowII :: (Integral t) => [t] -> t  
checkRowII elems = head results
  where results = [ x `div` y | x <- elems, y <- elems,  (x /= y) && (x `mod` y == 0) ]     

checkSumII :: [[Int]] -> Int
checkSumII = sum . map checkRowII

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ (show . checkSum . readRows) content
  putStrLn $ (show . checkSumII . readRows) content
