module Days.Day07 (solve1, solve2) where

import Data.Function ((&))
import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromJust)
import Data.Set (Set)
import Data.Set qualified as Set

solve1 :: String -> String
solve1 input =
  let (startState, levels) = parseInput input
   in calculateSplitNumber startState levels 0 & show

solve2 :: String -> String
solve2 input =
  let columnCount = input & lines & head & length
      (startState, splitterLevels) = parseInput input

      startIdx = head (Set.toList startState)

      initialCounts = [if columnIdx == startIdx then 1 else 0 | columnIdx <- [0 .. columnCount - 1]]

      resultCounts =
        foldl
          (handleLevel2 columnCount)
          initialCounts
          splitterLevels
   in show (sum resultCounts)

parseInput :: String -> (Set Int, [Set Int])
parseInput input = (startState, map Set.fromList splitterIndices)
  where
    (startRow : rows) = lines input
    startIdx = fromJust (elemIndex 'S' startRow)
    startState = Set.singleton startIdx
    splitterIndices = rows & map (elemIndices '^')

calculateSplitNumber :: Set Int -> [Set Int] -> Int -> Int
calculateSplitNumber initialIdxSet levels splitNumber = resultSplitNumber
  where
    (_, resultSplitNumber) = foldl handleLevel1 (initialIdxSet, splitNumber) levels

mapBeam :: Set Int -> Int -> [Int]
mapBeam levelIndicesSet beamIdx
  | Set.member beamIdx levelIndicesSet = [beamIdx - 1, beamIdx + 1]
  | otherwise = [beamIdx]

handleLevel1 :: (Set Int, Int) -> Set Int -> (Set Int, Int)
handleLevel1 (currentBeamIdxSet, splitNumber) splitterIdxSet = (Set.fromList newBeamIndices, splitNumber + newSplitNumber)
  where
    newBeamIndices = concatMap (mapBeam splitterIdxSet) currentBeamIdxSet
    newSplitNumber = length newBeamIndices - Set.size currentBeamIdxSet

handleLevel2 :: Int -> [Integer] -> Set Int -> [Integer]
handleLevel2 columnCount counts splitterIdxSet = [calculateNewCount columnIdx | columnIdx <- [0 .. columnCount - 1]]
  where
    getCountByIdx idx
      | idx < 0 || idx >= columnCount = 0
      | otherwise = counts !! idx

    isSplitter idx = Set.member idx splitterIdxSet

    calculateNewCount columnIdx = center + left + right
      where
        center = if isSplitter columnIdx then 0 else getCountByIdx columnIdx
        left = if columnIdx > 0 && isSplitter (columnIdx - 1) then getCountByIdx (columnIdx - 1) else 0
        right = if columnIdx + 1 < columnCount && isSplitter (columnIdx + 1) then getCountByIdx (columnIdx + 1) else 0