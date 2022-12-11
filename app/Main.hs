{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text    as T
import qualified Data.Text.IO as TIO
import qualified Day1
import qualified Day10
import qualified Day11
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Day8
import qualified Day9
import           Text.Printf

runDay :: Int -> String -> (String -> IO Int) -> (String -> IO Int) -> IO ()
runDay day file p1 p2 = do
  part1 <- p1 file
  part2 <- p2 file
  printf "Day%d : %d, %d\n" day part1 part2

runIDay ::
     Int -> String -> (String -> IO Integer) -> (String -> IO Integer) -> IO ()
runIDay day file p1 p2 = do
  part1 <- p1 file
  part2 <- p2 file
  printf "Day%d : %d, %d\n" day part1 part2

runTextDay ::
     Int -> String -> (String -> IO [Char]) -> (String -> IO [Char]) -> IO ()
runTextDay day file p1 p2 = do
  part1 <- p1 file
  part2 <- p2 file
  printf $ concat ["Day", show day, " : ", part1, ", ", part2, "\n"]

main :: IO ()
main = do
  runDay 1 "data/full/day1" Day1.part1 Day1.part2
  runDay 2 "data/full/day2" Day2.part1 Day2.part2
  runDay 3 "data/full/day3" Day3.part1 Day3.part2
  runDay 4 "data/full/day4" Day4.part1 Day4.part2
  runTextDay 5 "data/full/day5" Day5.part1 Day5.part2
  runDay 6 "data/full/day6" Day6.part1 Day6.part2
  runDay 7 "data/full/day7" Day7.part1 Day7.part2
  runDay 8 "data/full/day8" Day8.part1 Day8.part2
  runDay 9 "data/full/day9" Day9.part1 Day9.part2
  part10_1 <- Day10.part1 "data/full/day10"
  printf $ "Day10 Part1:" <> show part10_1 <> "\n"
  TIO.putStrLn "Day10 Part2:"
  Day10.part2 "data/full/day10"
  runIDay 11 "data/full/day11" Day11.part1 Day11.part2
