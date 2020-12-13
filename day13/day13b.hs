module Day13b where

-- (position, busID)
-- I got lazy and just used CRT in mathematica

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy c s = let (x, xs) = break (== c) s in x : (splitBy c $ drop 1 xs)

solve :: String -> IO ()
solve file = do
    x <- readFile file
    let ls = lines x
    let buses = splitBy ',' $ head $ tail ls
    let bustimes = map (\(i, x) -> (i, read x :: Int)) $ filter (\(_, x) -> x /= "x") $ zip [0..length buses - 1] buses
    print $ bustimes