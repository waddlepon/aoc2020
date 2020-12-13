module Main where

import Data.Maybe

data Instruction = NOP Int | ACC Int | JMP Int

toInstruction :: String -> Instruction
toInstruction x
    | i == "nop" = NOP n
    | i == "acc" = ACC n
    | i == "jmp" = JMP n
    where 
        (i:v) = words x
        n = read $ filter (/= '+') $ head v

testChanges :: [Instruction] -> Int
testChanges program = head $ mapMaybe (runProgram . changeIndex program) [0..(length program - 1)]
    where 
        changeIndex p i = let (f, x:xs) = splitAt i p in f ++ [change x] ++ xs
        change (NOP n) = JMP n 
        change (JMP n) = NOP n
        change i       = i

runProgram :: [Instruction] -> Maybe Int
runProgram program = runProgram_ program [] 0 0

runProgram_ :: [Instruction] -> [Int] -> Int -> Int -> Maybe Int
runProgram_ program indexes eip acc
        | eip `elem` indexes = Nothing
        | eip >= length program = Just acc
        | otherwise = case program !! eip of
            NOP _ -> runProgram_ program (eip : indexes) (eip + 1) acc
            ACC x -> runProgram_ program (eip : indexes) (eip + 1) (acc + x)
            JMP x -> runProgram_ program (eip : indexes) (eip + x) acc

main :: IO ()
main = do
    x <- getContents
    let program = map toInstruction $ lines x
    putStrLn . show $ testChanges program