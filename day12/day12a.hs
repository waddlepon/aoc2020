module Day12a where

import Control.Monad.State
import Data.List
import Data.Maybe

type Position = (Int, Int)

data Instruction = North Int | South Int | East Int | West Int | Forward Int | TurnLeft Int | TurnRight Int

directions = [(1, 0), (0, -1), (-1, 0), (0, 1)]

changeDirection :: Int -> Position -> Position
changeDirection v pos = this_directions !! (abs v + (fromJust $ elemIndex pos this_directions))
    where this_directions = if v < 0 then cycle $ reverse directions else cycle directions

processInstruction :: Instruction -> State Position Position
processInstruction (North v) = pure (0, v)
processInstruction (South v) = pure (0, -v)
processInstruction (East v) = pure (v, 0)
processInstruction (West v) = pure (-v, 0)
processInstruction (Forward v) = do
    (dx, dy) <- get
    pure (dx * v, dy * v)
processInstruction (TurnRight v) = do
    modify (changeDirection $ v `div` 90)
    pure (0, 0)
processInstruction (TurnLeft v) = do
    modify (changeDirection $ -v `div` 90)
    pure (0, 0)

toInstruction :: String -> Instruction
toInstruction x
    | i == 'N' = North v
    | i == 'S' = South v
    | i == 'E' = East v
    | i == 'W' = West v
    | i == 'F' = Forward v
    | i == 'L' = TurnLeft v
    | i == 'R' = TurnRight v
    where (i, v) = (head x, read $ tail x)

manhattanDistance :: Position -> Int
manhattanDistance (x, y) = abs x + abs y

solve :: String -> IO ()
solve file = do
    x <- readFile file
    let ls = lines x
    let dls = fst $ (runState $ sequence . map (processInstruction . toInstruction) $ ls) (1, 0)
    let endpoint = foldr (\(x, y) (dx, dy) -> (x + dx, y + dy)) (0, 0) dls
    print $ dls
    print $ endpoint
    print $ manhattanDistance endpoint