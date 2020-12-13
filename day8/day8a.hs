module Main where

data Instruction = NOP | ACC Int | JMP Int

toInstruction :: String -> Instruction
toInstruction x
    | i == "nop" = NOP
    | i == "acc" = ACC n
    | i == "jmp" = JMP n
    where 
        (i:v) = words x
        n = read $ filter (/= '+') $ head v

runProgram :: [Instruction] -> Int
runProgram program = runProgram_ program [] 0 0

runProgram_ :: [Instruction] -> [Int] -> Int -> Int -> Int
runProgram_ program indexes eip acc
        | eip `elem` indexes = acc
        | otherwise = case program !! eip of
            NOP   -> runProgram_ program (eip : indexes) (eip + 1) acc
            ACC x -> runProgram_ program (eip : indexes) (eip + 1) (acc + x)
            JMP x -> runProgram_ program (eip : indexes) (eip + x) acc

main :: IO ()
main = do
    x <- getContents
    let program = map toInstruction $ lines x
    putStrLn . show $ runProgram program