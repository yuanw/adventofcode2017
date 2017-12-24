module Main where


import Data.Char
import Text.Parsec
import Text.Parsec.String

inverseCaptcha :: [Int] -> Int
inverseCaptcha = sum . checkSum

checkHeadTail :: (Eq a) => [a] -> [a]
checkHeadTail elems = if (head elems) == (last elems) then [head elems] else []

checksum :: (Eq a, Num a) => Int -> [a] -> a
checksum _ [] = 0
checksum offset ds = sum [d1 | (d1, d2) <- pairs ds, d1 == d2]
  where
    pairs :: [a] -> [(a, a)]
    pairs xs = zip xs (drop offset (xs ++ xs))

checkSum :: (Eq a) => [a] -> [a]
checkSum  elems
  | length elems < 2 = []
  | otherwise = [elems !! x | x <- [1 .. (length elems -1)], (elems !! x) == (elems !! (x - 1))]  ++ checkHeadTail elems

checkSumII :: (Eq a) => [a] -> [a]
checkSumII elems = [elems !! x | x <- [0 .. (n - 1)], (elems !! x) == (elems !! (x + n))]
  where n = (length elems) `div` 2

inverseCaptchaII :: [Int] -> Int
inverseCaptchaII = (\x -> x * 2) . sum . checkSumII

withInput :: FilePath -> Parser a -> IO a
withInput path p = do
  result <- parseFromFile (p <* eof) path
  either (error . show) return result

main :: IO ()
main = do
  content <- readFile "input.txt"
  withInput "input.txt" (many1 (digitToInt <$> digit) <* newline) >>= \digits -> do
    putStrLn "Part 1"
    print (checksum 1 digits)
    print (inverseCaptcha digits)
    putStrLn "Part 2"
    print (inverseCaptchaII digits)

