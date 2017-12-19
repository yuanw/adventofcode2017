module Main where


checkHeadTail :: (Eq a) => [a] -> [a]
checkHeadTail elems = if (head elems) == (last elems) then [head elems] else []

checkSum :: (Eq a) => [a] -> [a]
checkSum  elems
  | length elems < 2 = []
  | otherwise = [elems !! x | x <- [1 .. (length elems -1)], (elems !! x) == (elems !! (x - 1))]  ++ checkHeadTail elems

main :: IO ()
main = do
  putStrLn "hello world"
