module Days.Day05 (solve1, solve2) where

import Data.Function ((&))
import Data.List (sortOn)
import Data.List.Split (splitOn)

type Range = (Int, Int)

solve1 :: String -> String
solve1 input =
  input
    & parseInput
    & filterFreshIngridients
    & length
    & show

solve2 :: String -> String
solve2 input =
  ranges
    & mergeRanges
    & map getRangeLength
    & sum
    & show
  where
    (ranges, _) = input & parseInput
    getRangeLength (from, to) = to - from + 1

mergeRanges :: [Range] -> [Range]
mergeRanges ranges = reverse $ foldl step [] sorted
  where
    sorted = sortOn fst ranges

    step [] range = [range]
    step acc@((currentFrom, currentTo) : rest) (from, to)
      | from <= currentTo + 1 = (currentFrom, max currentTo to) : rest
      | otherwise = (from, currentTo) : acc

filterFreshIngridients :: ([Range], [Int]) -> [Int]
filterFreshIngridients (ranges, ingridientIds) = filter isIngridientFresh ingridientIds
  where
    isIngridientFresh ingridientId = any (inRange ingridientId) ranges

inRange :: (Ord a) => a -> (a, a) -> Bool
inRange x (from, to) = x >= from && x <= to

parseInput :: [Char] -> ([Range], [Int])
parseInput input = (ranges, numbers)
  where
    (before, after) = break (== "") (lines input)
    ranges = map parseRange before
    numbers = drop 1 after & map read

parseRange :: String -> Range
parseRange line = (from, to)
  where
    [from, to] =
      line
        & splitOn "-"
        & map read