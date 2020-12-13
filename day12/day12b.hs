module Day12b where

import Control.Monad.State
import Data.List
import Data.Maybe

type Position = (Int, Int)

data Instruction = North Int | South Int | East Int | West Int | Forward Int | TurnLeft Int | TurnRight Int

directions = [(1, 0), (0, -1), (-1, 0), (0, 1)]

addPair :: Position -> Position -> Position
addPair (x, y) (x2, y2) = (x + x2, y + y2)

rotateRight :: Int -> Position -> Position
rotateRight 0 (x, y) = (x, y)
rotateRight n (x, y) = rotateRight (n - 1) (y, -x)

rotateLeft :: Int -> Position -> Position
rotateLeft 0 (x, y) = (x, y)
rotateLeft n (x, y) = rotateLeft (n - 1) (-y, x)
 
processInstruction :: Instruction -> State Position Position
processInstruction (North v) = do
    modify (addPair (0, v))
    pure (0, 0)
processInstruction (South v) = do
    modify (addPair (0, -v))
    pure (0, 0)
processInstruction (East v) = do 
    modify (addPair (v, 0))
    pure (0, 0)
processInstruction (West v) = do
    modify (addPair (-v, 0))
    pure (0, 0)
processInstruction (Forward v) = do
    (dx, dy) <- get
    pure (dx * v, dy * v)
processInstruction (TurnRight v) = do
    modify $ rotateRight (v `div` 90)
    pure (0, 0)
processInstruction (TurnLeft v) = do
    modify $ rotateLeft (abs v `div` 90)
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
    let dls = fst $ (runState $ sequence . map (processInstruction . toInstruction) $ ls) (10, 1)
    let endpoint = foldr addPair (0, 0) dls
    print $ dls
    print $ endpoint
    print $ manhattanDistance endpoint