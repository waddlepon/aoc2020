module Day16a where

import qualified Data.Map as Map
import Data.Char
import Data.List

splitBy :: Char -> String -> [String]
splitBy _ "" = []
splitBy c s = let (x, xs) = break (== c) s in x : (splitBy c $ drop 1 xs)

ruleToMap :: String -> Map.Map String [(Int, Int)]
ruleToMap s = Map.singleton field [(read l1, read l2), (read b1, read b2)]
    where
        field = takeWhile (/= ':') s
        numbers = words $ dropWhile (not . isDigit) s
        first = head numbers
        (l1, _:l2) = span (/= '-')  first
        second = last numbers
        (b1, _:b2) = span (/= '-') second

ticketToList :: String -> [Int]
ticketToList s = map read $ splitBy ',' s

checkValid :: Map.Map String [(Int, Int)] -> Int -> Int
checkValid m i = if or $ map (withinBounds i) bounds then 0 else i
    where
        bounds = concat $ Map.elems m
        withinBounds x (l, u) = x >= l && x <= u

solve :: String -> IO ()
solve file = do
    x <- readFile file
    let ls = lines x
    let (rules, _:rest) = span (/= "") ls
    let rulesmap = Map.unions $ map ruleToMap rules
    let my_ticket = ticketToList $ rest !! 1
    let nearby_tickets = map ticketToList $ tail $ dropWhile (/= "nearby tickets:") ls
    let invalid = map (checkValid rulesmap) $ concat nearby_tickets
    print $ sum invalid