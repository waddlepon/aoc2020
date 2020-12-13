module Main where

import Data.List

joltDiffs :: [Int] -> [Int]
joltDiffs x = zipWith (-) (tail better) better
    where better = sort (0 : x ++ [maximum x + 3]) 

main :: IO ()
main = do
    x <- getContents
    let ls = map read $ lines x
    let diffs = joltDiffs ls
    print $ (length $ filter (== 3) diffs) * (length $ filter (== 1) diffs)