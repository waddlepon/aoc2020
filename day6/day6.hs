module Main where

import Data.List

linesToGroup :: [String] -> [String]
linesToGroup [] = []
linesToGroup s  = let (f, l) = break (== "") s in concat f : linesToGroup (drop 1 l)

countConsensus :: [String] -> Int
countConsensus [] = 0
countConsensus s  = let (f, l) = break (== "") s in (length $ foldr1 intersect f) + countConsensus (drop 1 l)

main :: IO ()
main = do
    x <- getContents
    let ls = linesToGroup $ lines x
    putStrLn . show . sum $ map (length . nub) ls
    putStrLn . show . countConsensus $ lines x