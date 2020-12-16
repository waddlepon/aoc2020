module Day16b where

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

checkValid :: Map.Map String [(Int, Int)] -> Int -> Bool
checkValid m i = or $ map (withinBounds i) bounds
    where
        bounds = concat $ Map.elems m
        withinBounds x (l, u) = x >= l && x <= u

checkValidTicket :: Map.Map String [(Int, Int)] -> [Int] -> Bool
checkValidTicket m t = all (== True) $ map (checkValid m) t

findFields :: Int -> [[Int]] -> [(String, [(Int, Int)])] -> [String]
findFields _ _ [] = []
findFields index tickets ((field, bounds):xs) = if all (== True) $ map (\x -> or $ map (withinBounds x) bounds) values then field : findFields index tickets xs else findFields index tickets xs
    where
        values = map (!! index) tickets
        withinBounds x (l, u) = x >= l && x <= u
    
reduceFieldList :: [[String]] -> [[String]]
reduceFieldList fs = if all (\x -> length x == 1) out then out else reduceFieldList out
    where
        out = map cullList fs
        cullList l
            | 1 == length l = l
            | not . null $ intersect l ones = l \\ intersect l ones
            | otherwise = l
        ones = filter (/= "") $ map (\x -> if length x == 1 then head x else "") fs

solve :: String -> IO ()
solve file = do
    x <- readFile file
    let ls = lines x
    let (rules, _:rest) = span (/= "") ls
    let rulesmap = Map.unions $ map ruleToMap rules
    let my_ticket = ticketToList $ rest !! 1
    let nearby_tickets = map ticketToList $ tail $ dropWhile (/= "nearby tickets:") ls
    let culled_tickets = filter (/= []) $ map (\x -> if checkValidTicket rulesmap x then x else []) nearby_tickets
    let possiblefield_list = map (\i -> findFields i culled_tickets $ Map.toList rulesmap) [0..length my_ticket - 1]
    let field_list = concat $ reduceFieldList possiblefield_list
    print field_list
    print $ product $ zipWith (\f t -> if "departure" `isInfixOf` f then t else 1) field_list my_ticket