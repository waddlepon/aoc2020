module Main where

import Data.Char
import Data.List

seatId :: String -> Int
seatId s = (toDec $ take 7 binary) * 8 + (toDec $ drop 7 binary) 
    where
        binary = map toBinary s
        toBinary c
            | c == 'B' = '1'
            | c == 'F' = '0'
            | c == 'R' = '1'
            | c == 'L' = '0'
            | otherwise = '0'

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

main :: IO ()
main = do
    x <- getContents
    let ls = lines x
    let seatIds = map seatId ls
    putStrLn . show . maximum $ seatIds
    putStrLn . show $ [85..890] \\ seatIds
