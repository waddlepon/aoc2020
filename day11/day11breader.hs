module Day11breader where

import Debug.Trace

import Data.Array
import Data.Maybe
import qualified Data.Map as Map
import Control.Monad.Reader

type Board = Array Int Char
type Position = (Int, Int)
type Global = (Map.Map Position [Position], Int, Int)

directions = [(x, y) | x <- [-1..1], y <- [-1..1], x /= 0 || y /= 0]

positionToInt :: Position -> Int -> Int
positionToInt (x, y) cols = y * cols + x

neighbors :: Board -> Int -> Int -> Position -> [Position]
neighbors board rows cols pos = mapMaybe (findSeat pos) directions
    where
        findSeat p dir 
            | x >= cols || y >= rows || x < 0 || y < 0 = Nothing
            | otherwise = if (board ! positionToInt newp cols) `elem` ['#', 'L'] then Just newp else findSeat newp dir
            where
                newp@(x, y) = addPair p dir
        addPair (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

exec :: Board -> Reader Global Board
exec board = pure $ listArray (0, rows * cols - 1) $ zipWith changeSquare (elems board) positions
    where
        (neighbormap, rows, cols) = ask
        occupiedNeighbors pos = length . filter (== True) . map (\x -> '#' == board ! positionToInt x cols) $ neighbormap Map.! pos
        changeSquare c pos
            | c == 'L' = if occupiedNeighbors pos == 0 then '#' else 'L'
            | c == '#' = if occupiedNeighbors pos >= 5 then 'L' else '#'
            | otherwise = c
        positions = [(x, y) | y <- [0..rows - 1], x <- [0..cols - 1]]
    
findEndState :: Board -> Reader Global Int
findEndState b
    | b == next = pure $ length $ filter (== '#') $ elems next
    | otherwise = findEndState next 
    where
        env = ask
        next = runReader (exec b) env
        
solve :: String -> IO ()
solve s = do
    x <- readFile s
    let ls = lines x 
    let (rows, cols) = (length ls, length $ head ls)
    let flat = concat ls
    let board = listArray (0, length flat - 1) $ flat
    let positions = [(x, y) | y <- [0..rows - 1], x <- [0..cols - 1]]
    let neighbormap = Map.fromList $ zip positions $ map (neighbors board rows cols) positions
    print $ runReader (findEndState board) (neighbormap, rows, cols)