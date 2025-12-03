module Main (main) where

import Days.Day01 qualified as D01
import Days.Day02 qualified as D02
import Days.Day03 qualified as D03
import System.Environment (getArgs)

-- cabal run aoc -- 1 inputs/day01.txt
main :: IO ()
main = do
  args <- getArgs
  (dayNumber, input) <- case args of
    [dayNumberStr, filePath] -> do
      inputText <- readFile filePath
      pure (read dayNumberStr, inputText)
    _ -> error "Wrong parameters"
  case runDay dayNumber input of
    Left err -> putStrLn err
    Right (part1Answer, part2Answer) -> do
      putStrLn $ "Part 1: " <> part1Answer
      putStrLn $ "Part 2: " <> part2Answer

runDay :: Int -> String -> Either String (String, String)
runDay n input = case n of
  1 -> Right (D01.solve1 input, D01.solve2 input)
  2 -> Right (D02.solve1 input, D02.solve2 input)
  3 -> Right (D03.solve1 input, D03.solve2 input)
  _ -> Left ("Unknown day: " <> show n)