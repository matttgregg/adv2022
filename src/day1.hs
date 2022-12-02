module Day1
  ( part1
  , part2
  ) where

import           Data.List.Split (splitWhen)
import           Data.Sort       (sort)

-- A simple max function
myMax :: [Integer] -> Integer
myMax vals = max' vals 0
  where
    max' [] n = n
    max' (x:xs) n
      | x > n = max' xs x
    max' (x:xs) n = max' xs n

-- Part1 Solution.
part1 :: String -> IO Integer
part1 file = do
  raw <- readFile file
  let grouped = splitWhen ((== 0) . length) $ lines raw
      intGrouped = map (map read) grouped
      maxCalories = myMax (map sum intGrouped)
  return maxCalories

-- Part2 Solution.
part2 :: String -> IO Integer
part2 file = do
  raw <- readFile file
  let grouped = splitWhen ((== 0) . length) $ lines raw
      intGrouped = map (map read) grouped
      sortedCalories = sort $ map sum intGrouped
      maxCalories = sum $ take 3 $ reverse sortedCalories
  return maxCalories
