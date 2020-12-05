module Main where

treesInPath :: Int -> Int -> [String] -> Int
treesInPath mx my = go mx my 0 
    where 
        go _ _ _ []       = 0
        go mx my x (t:tx) = (if (t !! x) == '#' then 1 else 0) + go mx my ((x + mx) `mod` length t) (drop (my - 1) tx)

main :: IO ()
main = do
    x <- getContents
    let ls = lines x
    putStrLn . show $ (treesInPath 1 1 ls * treesInPath 3 1 ls * treesInPath 5 1 ls * treesInPath 7 1 ls * treesInPath 1 2 ls)