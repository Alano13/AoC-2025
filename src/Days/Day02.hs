module Days.Day02 (solve1, solve2) where

import Data.Function ((&))
import Data.List.Split (splitOn)

solve1 :: String -> String
solve1 = solve isValidNumber1

solve2 :: String -> String
solve2 = solve isValidNumber2

solve :: (Int -> Bool) -> String -> String
solve isValidNumber input =
  parseInput input
    & filter (not . isValidNumber)
    & sum
    & show

parseInput :: String -> [Int]
parseInput input =
  input
    & splitOn ","
    & map (\pair -> splitOn "-" pair & map read)
    & concatMap (\[from, to] -> [from .. to])

isValidNumber1 :: Int -> Bool
isValidNumber1 number = left /= right
  where
    numberLength = getNumberLength number
    divider = 10 ^ (numberLength `div` 2)
    (left, right) = number `divMod` divider

isValidNumber2 :: Int -> Bool
isValidNumber2 number = all (checkNumber number) [x | x <- [1 .. numberLength `div` 2], numberLength `mod` x == 0]
  where
    numberLength = getNumberLength number

checkNumber :: Int -> Int -> Bool
checkNumber number base = go (number `divMod` divider) divider
  where
    divider = 10 ^ base
    go (0, _) _ = False
    go (x, remainder) d = ((x `mod` d) /= remainder) || go (x `divMod` d) d

getNumberLength :: Int -> Int
getNumberLength = go
  where
    go 0 = 0
    go y = 1 + go (y `div` 10)