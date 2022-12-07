{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Text    as T
import qualified Data.Text.IO as T.IO
import qualified Day1
import qualified Day2
import qualified Day3
import qualified Day4
import qualified Day5
import qualified Day6
import qualified Day7
import qualified Text.Printf  as TIO
import           Text.Printf

runDay :: Int -> String -> (String -> IO Int) -> (String -> IO Int) -> IO ()
runDay day file p1 p2 = do
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
  runDay 1 "data/day1" Day1.part1 Day1.part2
  runDay 2 "data/day2" Day2.part1 Day2.part2
  runDay 3 "data/day3" Day3.part1 Day3.part2
  runDay 4 "data/day4" Day4.part1 Day4.part2
  runTextDay 5 "data/day5" Day5.part1 Day5.part2
  runDay 6 "data/day6" Day6.part1 Day6.part2
  runDay 7 "data/day7" Day7.part1 Day7.part2
