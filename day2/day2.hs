module Main where

import Data.List
import Data.Char

validPass :: String -> Bool
validPass s = (lb <= ccount) && (ub >= ccount)
    where
        sx = words s
        crange = groupBy (const isDigit) $ head sx
        lb = read $ head crange
        ub = read $ tail $ last crange
        c = head $ sx !! 1
        pass = sx !! 2
        ccount = length $ filter (== c) pass

validPass2 :: String -> Bool
validPass2 s = (pass !! (lb - 1) == c) /= (pass !! (ub - 1) == c)
    where
        sx = words s
        crange = groupBy (const isDigit) $ head sx
        lb = read $ head crange
        ub = read $ tail $ last crange
        c = head $ sx !! 1
        pass = sx !! 2
      
main :: IO ()
main = do
    x <- getContents
    let ls = lines x
    putStrLn . show $ length . filter (==True) $ map validPass ls
    putStrLn . show $ length . filter (==True) $ map validPass2 ls