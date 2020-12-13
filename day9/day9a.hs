module Main where

badNumber :: [Int] -> Int
badNumber s = go (take 25 s) (drop 25 s)
    where
        go pre@(x:xs) (y:ys) = if testGood pre y then go (xs ++ [y]) ys else y
        testGood [] _     = False
        testGood (x:xs) n = (n - x `elem` xs) || testGood xs n

main :: IO ()
main = do
    x <- getContents
    let ls = map read $ lines x
    print $ badNumber ls