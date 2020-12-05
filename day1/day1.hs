module Main where

entrySum :: [Int] -> Int
entrySum s = head [x * y|x <- s, y <- s, x + y == 2020]

entrySumThree :: [Int] -> Int
entrySumThree s = head [x * y * z | x <- s, y <- s, z <- s, x + y + z== 2020]


main :: IO ()
main = do
   x <- getContents
   let ls = map read $ lines x
   putStrLn . show $ entrySum ls
   putStrLn . show $ entrySumThree ls