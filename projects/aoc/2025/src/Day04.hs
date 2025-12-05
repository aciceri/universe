module Day04 (part1, part2, parseGrid, processGrid) where

import Data.Function (fix)
import Data.Vector qualified as V

data Cell = Roll | Empty deriving (Show, Eq)

type Grid = V.Vector (V.Vector Cell)

parseCell :: Char -> Cell
parseCell c
  | c == '.' = Empty
  | otherwise = Roll

parseGrid :: String -> Grid
parseGrid = V.fromList . map (V.fromList . parseLine) . lines
  where
    parseLine = map parseCell

countAdjacentRolls :: Grid -> (Int, Int) -> Int
countAdjacentRolls grid (x, y) = sum [countRolls (m, n) | m <- [x - 1 .. x + 1], n <- [y - 1 .. y + 1]]
  where
    countRolls (m, n)
      | (m, n) == (x, y) = 0
      | m < 0 = 0
      | n < 0 = 0
      | m > width - 1 = 0
      | n > height - 1 = 0
      | (grid V.! n) V.! m == Roll = 1
      | otherwise = 0
    width = V.length (grid V.! 0)
    height = V.length grid

countGoodCells :: Grid -> Int
countGoodCells grid = sum [isGoodCell (m, n) | m <- [0 .. width - 1], n <- [0 .. height - 1]]
  where
    isGoodCell (x, y)
      | (grid V.! y) V.! x == Empty = 0
      | adjacentCells < 4 = 1
      | otherwise = 0
      where
        adjacentCells = countAdjacentRolls grid (x, y)
    width = V.length (grid V.! 0)
    height = V.length grid

part1 :: String -> Either () Int
part1 = Right . countGoodCells . parseGrid

processGrid :: (Int, Grid) -> (Int, Grid)
processGrid (removed, grid) = (removed + removedRolls, newGrid)
  where
    -- Not efficient, we are iterating over the matrix two times,
    -- however this way I can re-use the function written for part 1
    removedRolls = countGoodCells grid
    newGrid = V.fromList [V.fromList [transformCell (m, n) | n <- [0 .. width - 1]] | m <- [0 .. height - 1]]
    transformCell (x, y)
      | adjacentCells < 4 = Empty
      | otherwise = (grid V.! y) V.! x
      where
        adjacentCells = countAdjacentRolls grid (x, y)
    width = V.length (grid V.! 0)
    height = V.length grid

part2 :: String -> Either () Int
part2 = Right . fst . until noChange processGrid . (0,) . parseGrid
  where
    -- possible bug: why does it loop if I simpy check if `processGrid state == state`?
    noChange state = (fst . processGrid) state == fst state
