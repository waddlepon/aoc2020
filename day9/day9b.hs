module Main where

import Data.List

badNumber :: [Int] -> Int
badNumber s = go (take 25 s) (drop 25 s)
    where
        go pre@(x:xs) (y:ys) = if testGood pre y then go (xs ++ [y]) ys else y
        testGood [] _     = False
        testGood (x:xs) n = (n - x `elem` xs) || testGood xs n

weakness :: [Int] -> Int
weakness s = maximum range + minimum range
    where
        range = head $ filter ((== badn) . sum) $ concatMap tails $ inits $ filter (< badn) s
        badn = badNumber s

main :: IO ()
main = do
    x <- getContents
    let ls = map read $ lines x
    print $ weakness ls