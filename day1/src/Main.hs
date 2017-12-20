module Main where


import qualified Data.Char as DChar (digitToInt)    

inverseCaptcha :: String -> Int
inverseCaptcha = sum . (map DChar.digitToInt) . checkSum

checkHeadTail :: (Eq a) => [a] -> [a]
checkHeadTail elems = if (head elems) == (last elems) then [head elems] else []

checkSum :: (Eq a) => [a] -> [a]
checkSum  elems
  | length elems < 2 = []
  | otherwise = [elems !! x | x <- [1 .. (length elems -1)], (elems !! x) == (elems !! (x - 1))]  ++ checkHeadTail elems

checkSumII :: (Eq a) => [a] -> [a] 
checkSumII elems = [elems !! x | x <- [0 .. (n - 1)], (elems !! x) == (elems !! (x + n))]
  where n = (length elems) `div` 2

inverseCaptchaII :: String -> Int
inverseCaptchaII = (\x -> x * 2) . sum . (map DChar.digitToInt) . checkSumII  

main :: IO ()
main = do
  content <- readFile "input.txt"
  putStrLn $ (show . inverseCaptcha) content
  putStrLn $ (show . inverseCaptchaII) content


