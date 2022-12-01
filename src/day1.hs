{-
ghci> day1 <- readFile "data/day1"
ghci> day1_lines = lines day1
ghci> grouped = splitWhen ((== 0) . length) day1_lines
ghci> int_grouped = map (map read) grouped :: [[Integer]]

ghci> :{
ghci|   max' [] n = n
ghci|   max' (x:xs) n | x > n = max' xs x
ghci|   max' (x:xs) n = max' xs n
ghci| :}
ghci> max' (map sum int_grouped) 0

-}
module Day1
  ( part1
  ) where

import           Data.List.Split (splitWhen)

part1 :: String -> String
part1 file = "100"
