{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Days.Day04 (solve1, solve2) where

import Data.Foldable (foldl')
import Data.Function ((&))

type Matrix = [[Char]]

type Cell = (Int, Int)

data Grid = Grid
  { matrix :: Matrix,
    colCount :: Int,
    rowCount :: Int
  }

solve1 :: String -> String
solve1 input =
  input
    & lines
    & getAvailableCells
    & length
    & show

solve2 :: String -> String
solve2 input =
  let matrix = lines input
      oldRollCount = getRollCount matrix
      newRollCount = getRollCount (removeAllAvailableRolls matrix)
   in show (oldRollCount - newRollCount)

getAvailableCells :: [[Char]] -> [Cell]
getAvailableCells matrix =
  let grid = createGrid matrix
   in [ (rowIdx, colIdx)
        | rowIdx <- [0 .. (grid.rowCount - 1)],
          colIdx <- [0 .. (grid.colCount - 1)],
          cellHasAvailableRoll grid (rowIdx, colIdx)
      ]

createGrid :: [[Char]] -> Grid
createGrid matrix = Grid {matrix = matrix, rowCount = length matrix, colCount = length (head matrix)}

cellHasAvailableRoll :: Grid -> Cell -> Bool
cellHasAvailableRoll grid (rowIdx, colIdx) = currentCellHasRoll && length adjacentRollsCount < 4
  where
    currentCellHasRoll = hasRoll grid (rowIdx, colIdx)
    adjacentRollsCount =
      [ True
        | x <- [-1 .. 1],
          y <- [-1 .. 1],
          not (x == 0 && y == 0),
          hasRoll grid (rowIdx + x, colIdx + y)
      ]

hasRoll :: Grid -> Cell -> Bool
hasRoll grid (rowIdx, colIdx)
  | colIdx >= grid.colCount || colIdx < 0 = False
  | rowIdx >= grid.rowCount || rowIdx < 0 = False
  | otherwise = grid.matrix !! rowIdx !! colIdx == '@'

getRollCount :: Matrix -> Int
getRollCount matrix =
  matrix
    & concatMap (filter (== '@'))
    & length

removeAllAvailableRolls :: Matrix -> Matrix
removeAllAvailableRolls matrix = resultMatrix
  where
    cellsToRemove = getAvailableCells matrix
    resultMatrix
      | null cellsToRemove = matrix
      | otherwise = removeAllAvailableRolls (removeRolls matrix cellsToRemove)

removeRolls :: Matrix -> [Cell] -> Matrix
removeRolls = foldl' (\x (rowIdx, colIdx) -> updateMatrixAt (rowIdx, colIdx) '.' x)

updateMatrixAt :: Cell -> Char -> Matrix -> Matrix
updateMatrixAt (rowIdx, colIdx) newValue matrix =
  case splitAt rowIdx matrix of
    (beforeRows, rowList : afterRows) ->
      let newRow = replaceAt colIdx newValue rowList
       in beforeRows ++ newRow : afterRows
    _ -> matrix

replaceAt :: Int -> a -> [a] -> [a]
replaceAt idx newValue xs =
  case splitAt idx xs of
    (before, _ : after) -> before ++ newValue : after
    _ -> xs
