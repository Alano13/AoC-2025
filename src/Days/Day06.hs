module Days.Day06 (solve1, solve2) where

import Data.Function ((&))
import Data.List (transpose)

data Operator = Add | Multiply
  deriving (Show)

type Expression = ([Int], Operator)

solve1 :: String -> String
solve1 input =
  input
    & parseInput
    & map calculateExpression
    & sum
    & show

solve2 :: String -> String
solve2 input =
  input
    & lines
    & parseInputAsTable
    & map calculateExpression
    & sum
    & show

parseInput :: String -> [Expression]
parseInput input =
  input
    & lines
    & map words
    & transpose
    & map parseLine

parseLine :: [String] -> Expression
parseLine line = (numbers, operator)
  where
    numbers = init line & map read
    operator = last line & parseOperator

parseOperator :: String -> Operator
parseOperator "*" = Multiply
parseOperator "+" = Add

calculateExpression :: Expression -> Int
calculateExpression (numbers, Add) = sum numbers
calculateExpression (numbers, Multiply) = product numbers

parseInputAsTable :: [String] -> [Expression]
parseInputAsTable rows =
  let width = rows & map length & maximum
      pad row = row ++ replicate (width - length row) ' '
      columns = rows & map pad & transpose
      blocks = splitBlocks columns
   in map parseBlock blocks

splitBlocks :: [String] -> [[String]]
splitBlocks xs =
  case dropWhile isBlankColumn xs of
    [] -> []
    ys ->
      let (block, rest) = break isBlankColumn ys
       in block : splitBlocks rest

parseBlock :: [String] -> Expression
parseBlock columns =
  let operator = parseOperator [last (head columns)]
      numbers = map parseNumber (reverse columns)
   in (numbers, operator)

parseNumber :: String -> Int
parseNumber column = read strNumber :: Int
  where
    digits = init column
    strNumber = filter (/= ' ') digits

isBlankColumn :: String -> Bool
isBlankColumn = all (== ' ')