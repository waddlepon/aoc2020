module Main where

import qualified Data.Map as Map
import Data.List
import Data.Char

countBags :: String -> Map.Map String [(Int, String)] ->  Int
countBags s m = case Map.lookup s m of
    Nothing -> 0
    Just a -> sum $ map (\x -> fst x + fst x * (countBags (snd x) m)) a

rulesToMap :: [String] -> Map.Map String [(Int, String)]
rulesToMap r = Map.unions $ map stringToBags $ filter (\x -> not $ "no other" `isInfixOf` x) r

stringToBags :: String -> Map.Map String [(Int, String)]
stringToBags s = let (x:xs) = stringToBags_ $ filter (/= "contain") $ words s in Map.singleton x $ map toCountBag xs
    where toCountBag b = let (n, c) = span isDigit b in (read n :: Int, c)

stringToBags_ :: [String] -> [String]
stringToBags_ [] = []
stringToBags_ x  = let (k, l) = break (\x -> "bag" `isInfixOf` x) x in concat k : stringToBags_ (drop 1 l)

main :: IO ()
main = do
    x <- getContents
    let ls = lines x
    putStrLn . show $ countBags "shinygold" $ rulesToMap ls 