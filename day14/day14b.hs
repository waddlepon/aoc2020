module Day14B where

import qualified Data.IntMap as IntMap
import Control.Monad.State
import Data.Char
import Data.List
import Data.Maybe
import Numeric (showHex, showIntAtBase)

type Memory = IntMap.IntMap Int
type Mask = String

intToBinary :: Int -> String
intToBinary i = replicate (36 - length out) '0' ++ out
    where
        out = showIntAtBase 2 intToDigit i ""

binaryToInt :: String -> Int
binaryToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

posAddr :: String -> [String]
posAddr s
    | 'X' `elem` s = (posAddr $ before ++ ['0'] ++ after) ++ (posAddr $ before ++ ['1'] ++ after)
    | otherwise = [s]
    where
        i = fromJust $ elemIndex 'X' s
        (before, _:after) = splitAt i s

addValue :: (Int, Int) -> State Mask Memory
addValue (i, v) = do
    mask <- get
    let bi = intToBinary i 
    let maskon x y = if x == '0' then y else x
    let maskedbi = zipWith maskon mask bi
    let addr = map binaryToInt $ posAddr maskedbi
    pure . IntMap.unions $ map (`IntMap.singleton` v) addr

changeMask :: Mask -> State Mask Memory
changeMask mask = do 
    put mask
    pure IntMap.empty

stringToInstruction :: String -> State Mask Memory
stringToInstruction s
    | inst == "mask" = changeMask $ words s !! 2
    | otherwise = addValue (read $ takeWhile isDigit $ drop 4 s, read $ words s !! 2)
    where
        inst = take 4 s

solve :: String -> IO ()
solve file = do
    x <- readFile file
    let ls = map stringToInstruction $ lines x
    let endMemory = foldl' (flip IntMap.union) IntMap.empty $ evalState (sequence ls) ""
    print $ sum $ IntMap.elems endMemory

