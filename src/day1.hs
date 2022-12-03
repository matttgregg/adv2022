module Day1
  ( part1
  , part2
  ) where

import           Data.List.Split (splitWhen)
import           Data.Sort       (sort)

-- A simple max function
myMax :: [Int] -> Int
myMax vals = max' vals 0
  where
    max' [] n = n
    max' (x:xs) n
      | x > n = max' xs x
    max' (x:xs) n = max' xs n

-- Part1 Solution.
part1 :: String -> IO Int
part1 = (findMaxCalories <$>) . readFile
  where
    findMaxCalories =
      myMax . map sum . map (map read) . splitWhen ((== 0) . length) . lines

-- Part2 Solution.
part2 :: String -> IO Int
part2 = (findMaxThree <$>) . readFile
  where
    findMaxThree =
      sum .
      take 3 .
      reverse . map sum . map (map read) . splitWhen ((== 0) . length) . lines
