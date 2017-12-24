module Day04
    ( day04
    , isValidStrongPasspharse
    ) where

import qualified Data.Set as Set
import qualified Data.List as List


isVaildPassphrase :: [String] -> Bool
isVaildPassphrase elems = length elems == length (Set.fromList elems)

isValidStrongPasspharse :: [String] -> Bool
isValidStrongPasspharse elems = length elems == (length . Set.fromList. map List.sort) elems

day04 :: IO ()
day04 = do
    content <- readFile "input4.txt"
    putStrLn "Part I"
    print $  ( length . filter ( isVaildPassphrase . words) . lines) content
    putStrLn "Part II"
    print $  ( length . filter ( isValidStrongPasspharse . words) . lines) content
