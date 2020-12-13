module Main where

import Data.List
import Data.Char

import qualified Data.Map as Map

processRules :: [String] -> [String]
processRules s = (nub $ findBags "shinygold") \\ ["shinygold"]
    where
        culled_s = map words $ filter (\x -> not $ "contains no other" `isInfixOf` x) s
        bag_map = Map.unionsWith (++) $ map (toSingletons . stringToBags) culled_s
        findBags b = case Map.lookup b bag_map of
            Nothing -> [b]
            Just x -> b : (concat $ map findBags x)
        
toSingletons :: [String] -> Map.Map String [String]
toSingletons x = Map.unions $ map toSingleton $ tail x
    where 
        toSingleton b = Map.singleton b [head x]

stringToBags :: [String] -> [String]
stringToBags = stringToBags_ . filter (\x -> not $ x == "contain" || all isDigit x)

stringToBags_ :: [String] -> [String]
stringToBags_ [] = []
stringToBags_ x  = let (k, l) = break (\x -> "bag" `isInfixOf` x) x in concat k : stringToBags (drop 1 l)

main :: IO ()
main = do
    x <- getContents
    let ls = lines x
    putStrLn . show . length $ processRules ls