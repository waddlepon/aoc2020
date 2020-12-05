module Main where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe

import Text.Regex

eyecolors = ["amb", "blu" ,"brn", "gry", "grn", "hzl", "oth"]

fields = Map.fromList [("byr", (\x -> length x == 4 && readNumber x >= 1920 && readNumber x <= 2002)),
    ("iyr", (\x -> length x == 4 && readNumber x >= 2010 && readNumber x <= 2020)),
    ("eyr", (\x -> length x == 4 && readNumber >= 2020 && readNumber x <= 2030)),
    ("hgt", (\x -> x ~= )),
    "hcl",
    ("ecl", (\x -> x `elem` eyecolors)),
     ("pid", (\x -> readNumber x /= -1 && length x == 9))]

readNumber s = case readMaybe s of
    Just x -> x
    Nothing -> -1

checkValid :: String -> Bool
checkValid p = 
    where
        x = Set.fromList $ map getField $ words p
        getField s = let (f, v) = splitAt (fromJust $ elemIndex ':' s) s in (f, tail v)
        checkValidField (field, value) = 


dumbTail [] = []
dumbTail x  = tail x

linesToPassports :: [String] -> [String]
linesToPassports [] = []
linesToPassports x  = intercalate " " (takeWhile (/= "") x) : (linesToPassports $ dumbTail $ dropWhile (/= "") x)

main :: IO ()
main = do
    x <- getContents
    let ls = linesToPassports $ lines x
    putStrLn $ unlines ls
    putStrLn . show $ length $ filter (== True) $ map checkValid ls
