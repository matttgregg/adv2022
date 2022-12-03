module Main where

import qualified Day1
import qualified Day2
import qualified Day3
import           Text.Printf

runDay :: Int -> String -> (String -> IO Int) -> (String -> IO Int) -> IO ()
runDay day file p1 p2 = do
  part1 <- p1 file
  part2 <- p2 file
  printf "Day%d : %d, %d\n" part1 part2

main :: IO ()
main = do
  runDay 1 "data/day1" Day1.part1 Day1.part2
  runDay 2 "data/day2" Day2.part1 Day2.part2
  runDay 3 "data/day3" Day3.part1 Day3.part2
