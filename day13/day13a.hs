module Day13a where

import Data.List
import Data.Ord ( comparing )

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy c s = let (x, xs) = break (== c) s in x : (splitBy c $ drop 1 xs)

earliestTime :: Int -> [Int] -> (Int, Int)
earliestTime t buses = minimumBy (comparing snd) $ zip buses $ map waitTime buses
    where
        waitTime bus = bus - t `mod` bus

solve :: String -> IO ()
solve file = do
    x <- readFile file
    let ls = lines x
    let mytime = read $ head ls
    let buses = map read $ filter (/= "x") $ splitBy ',' $ head $ tail ls
    print $ earliestTime mytime buses