module Days.Day03 (solve1, solve2) where

import Data.Char (digitToInt)
import Data.Function ((&))

solve1 :: String -> String
solve1 = solve 2

solve2 :: String -> String
solve2 = solve 12

solve :: Int -> String -> String
solve batteriesNumber input =
  input
    & lines
    & map (map digitToInt)
    & map (calculateMaxJoltage batteriesNumber)
    & sum
    & show

calculateMaxJoltage :: Int -> [Int] -> Int
calculateMaxJoltage n joltages = digitsToNumber (chooseMaxJoltages joltages)
  where
    chooseMaxJoltages xs
      | length xs == n = xs
      | otherwise = chooseMaxJoltages (removeSuboptimal xs)

removeSuboptimal :: [Int] -> [Int]
removeSuboptimal (x : y : xs)
  | x < y = y : xs
  | otherwise = x : removeSuboptimal (y : xs)
removeSuboptimal _ = []

digitsToNumber :: [Int] -> Int
digitsToNumber = foldl (\acc digit -> acc * 10 + digit) 0