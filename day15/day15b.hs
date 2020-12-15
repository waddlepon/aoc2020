module Day15b where

import qualified Data.IntMap as IntMap

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy c s = let (x, xs) = break (== c) s in x : (splitBy c $ drop 1 xs)

getAnswer :: [Int] -> Int
getAnswer ns = go (last ns) (length ns) $ IntMap.fromList $ zip (init ns) [1..]
    where
        go n i m 
            | i == 30000000 = n
            | otherwise = case IntMap.lookup n m of
                Nothing -> go 0 (i + 1) $ IntMap.insert n i m
                Just x -> go (i - x) (i + 1) $ IntMap.insert n i m

solve :: String -> IO ()
solve file = do
    x <- readFile file
    let ns = map read $ splitBy ',' $ head $ lines x 
    print $ getAnswer ns
    