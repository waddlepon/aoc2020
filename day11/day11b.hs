module Day11b where

import Debug.Trace

import Data.Array
import Data.Maybe
import qualified Data.Map as Map

type Board = (Array Int Char, Int, Int)
type Position = (Int, Int)

directions = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

positionToInt :: Board -> Position -> Int
positionToInt (_, _, cols) (x, y) = y * cols + x

neighbors :: Board -> Position -> [Position]
neighbors b@(board, rows, cols) pos = mapMaybe (findSeat pos) directions
    where
        findSeat p dir 
            | x >= cols || y >= rows || x < 0 || y < 0 = Nothing
            | otherwise = if (board ! positionToInt b newp) `elem` ['#', 'L'] then Just newp else findSeat newp dir
            where
                newp@(x, y) = addPair p dir
        addPair (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

exec :: Board -> Map.Map Position [Position] -> Board
exec b@(board, rows, cols) neighbormap = (listArray (0, rows * cols - 1) $ zipWith changeSquare (elems board) positions, rows, cols)
    where
        occupiedNeighbors pos = length . filter (== True) . map (\x -> '#' == board ! positionToInt b x) $ neighbormap Map.! pos
        changeSquare c pos
            | c == 'L' = if occupiedNeighbors pos == 0 then '#' else 'L'
            | c == '#' = if occupiedNeighbors pos >= 5 then 'L' else '#'
            | otherwise = c
        positions = [(x, y) | y <- [0..rows - 1], x <- [0..cols - 1]]
    
findEndState :: Board -> Map.Map Position [Position] -> Int
findEndState b neighbormap
    | b == next = length $ filter (== '#') $ elems board
    | otherwise = findEndState next neighbormap
    where
        next@(board, _, _) = exec b neighbormap
        
solve :: String -> IO ()
solve s = do
    x <- readFile s
    let ls = lines x 
    let (rows, cols) = (length ls, length $ head ls)
    let flat = concat ls
    let board = (listArray (0, length flat - 1) $ flat, rows, cols)
    let positions = [(x, y) | y <- [0..rows - 1], x <- [0..cols - 1]]
    let neighbormap = Map.fromList $ zip positions $ map (neighbors board) positions
    print $ findEndState board neighbormap