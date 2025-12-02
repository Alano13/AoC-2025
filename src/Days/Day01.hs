module Days.Day01 (solve1, solve2) where

import Data.Function ((&))

solve1 :: String -> String
solve1 = solve rotate1

solve2 :: String -> String
solve2 = solve rotate2

solve :: (Int -> String -> (Int, Int)) -> String -> String
solve rotateFunction input =
  input
    & lines
    & countZeros rotateFunction
    & show

countZeros :: (Int -> String -> (Int, Int)) -> [String] -> Int
countZeros countZerosForLine = fst . foldl helper (0, 50)
  where
    helper (resultZeroNumber, oldState) currentStr =
      let (state, number) = countZerosForLine oldState currentStr
       in (resultZeroNumber + number, state)

rotate1 :: Int -> String -> (Int, Int)
rotate1 currentDialState (direction : numberStr) = (newDialState, zeroNumber)
  where
    sign = getDirectionSign direction
    number = read numberStr
    newDialState = (currentDialState + (sign * number)) `mod` 100
    zeroNumber = if newDialState == 0 then 1 else 0
rotate1 _ [] = error "Wrong format"

rotate2 :: Int -> String -> (Int, Int)
rotate2 currentDialState (direction : numberStr) = (newDialState, zeroNumber)
  where
    sign = getDirectionSign direction
    number = read numberStr
    newDialState = (currentDialState + (sign * number)) `mod` 100
    fullTurns = number `div` 100
    remainedSteps = number `mod` 100

    distanceToZero =
      if currentDialState == 0
        then 100
        else case direction of
          'R' -> 100 - currentDialState
          'L' -> currentDialState
          _ -> error "Wrong format"

    extraZero = if remainedSteps >= distanceToZero then 1 else 0

    zeroNumber = fullTurns + extraZero
rotate2 _ [] = error "Wrong format"

getDirectionSign :: Char -> Int
getDirectionSign 'L' = -1
getDirectionSign 'R' = 1
getDirectionSign _ = error "Wrong direction"