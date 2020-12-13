module Main where

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Maybe
import Text.Read

eyecolors = ["amb", "blu" ,"brn", "gry", "grn", "hzl", "oth"]

fields :: Map.Map [Char] ([Char] -> Bool)
fields = Map.fromList [("byr", (\x -> length x == 4 && readNumber x >= 1920 && readNumber x <= 2002)),
    ("iyr", (\x -> length x == 4 && readNumber x >= 2010 && readNumber x <= 2020)),
    ("eyr", (\x -> length x == 4 && readNumber x >= 2020 && readNumber x <= 2030)),
    ("hgt", checkValidHeight),
    ("hcl", (\x -> (head x == '#') && (length $ tail x) == 6 && all (`elem` "abcdef0123456789") (tail x))),
    ("ecl", (\x -> x `elem` eyecolors)),
    ("pid", (\x -> all (`elem` "0123456789") x && length x == 9)),
    ("cid", (const True))]

tags = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

readNumber s = case readMaybe s of
    Just x -> x
    Nothing -> -1

checkValidHeight :: String -> Bool
checkValidHeight s 
    | postfix == "cm" = 150 <= readNumber prefix && readNumber prefix <= 193
    | postfix == "in" = 59 <= readNumber prefix && readNumber prefix <= 76
    | otherwise = False
    where
        (prefix, postfix) = splitAt (length s - 2) s


checkValid :: String -> Bool
checkValid p = all checkValidField x && tags == (intersect tags $ map fst $ Set.toList x)
    where
        x = Set.fromList $ map getField $ words p
        getField s = let (f, v) = splitAt (fromJust $ elemIndex ':' s) s in (f, tail v)
        checkValidField (field, value) = case Map.lookup field fields of
            Just f -> f value
            Nothing -> False


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
