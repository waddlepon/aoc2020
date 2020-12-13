module Main where

import Data.List
import Data.Maybe

import qualified Data.IntMap as IntMap

findArrangements :: [Int] -> IntMap.IntMap Int -> IntMap.IntMap Int
findArrangements [] m = m
findArrangements s@(x:xs) m = findArrangements xs newMap 
    where
        nextSteps (x:xs) = takeWhile (<= x + 3) xs
        amountSteps = case IntMap.lookup x m of
            Nothing -> 0
            Just x -> x
        newMap = foldl' (\nm t -> IntMap.insertWith (+) t amountSteps nm) m $ nextSteps s

main :: IO ()
main = do
    x <- getContents
    let ls = map (\x -> read x :: Int) $ lines x
    let better = sort (0 : ls ++ [maximum ls + 3])
    print $ fromJust $ findArrangements better (IntMap.fromList [(0, 1)]) IntMap.!? (maximum ls)
