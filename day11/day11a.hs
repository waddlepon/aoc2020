module Day11a where

import Debug.Trace

import Data.Array

type Board = (Array Int Char, Int, Int)
type Position = (Int, Int)

positionToInt :: Board -> Position -> Int
positionToInt (_, _, cols) (x, y) = y * cols + x

neighbors :: Board -> Position -> [Position]
neighbors (_, rows, cols) (x, y) = [(x1, y1) | x1 <- [x - 1 .. x + 1], y1 <- [y - 1 .. y + 1], x1 /= x || y1 /= y, x1 < cols, y1 < rows, x1 >= 0, y1 >= 0] 

exec :: Board -> Board
exec b@(board, rows, cols) = (listArray (0, rows * cols - 1) $ zipWith changeSquare (elems board) positions, rows, cols)
    where
        occupiedNeighbors pos = length . filter (== True) . map (\x -> '#' == board ! positionToInt b x) $ neighbors b pos
        changeSquare c pos
            | c == 'L' = if occupiedNeighbors pos == 0 then '#' else 'L'
            | c == '#' = if occupiedNeighbors pos >= 4 then 'L' else '#'
            | otherwise = c
        positions = [(x, y) | y <- [0..rows - 1], x <- [0..cols - 1]]
    
findEndState :: Board -> Int
findEndState b
    | b == next = length $ filter (== '#') $ elems board
    | otherwise = findEndState next
    where
        next@(board, _, _) = exec b
        
solve :: String -> IO ()
solve s = do
    x <- readFile s
    let ls = lines x 
    let (rows, cols) = (length ls, length $ head ls)
    let flat = concat ls
    let board = (listArray (0, length flat - 1) $ flat, rows, cols)
    print $ findEndState board