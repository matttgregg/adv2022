module Day3
  ( part1
  , part2
  ) where

import           Data.Char       (ord)
import           Data.List       (intersect, nub, splitAt)
import           Data.List.Split (chunksOf)

priority :: Char -> Int
priority = priority' . ord
  where
    priority' c
      | c >= 97 = c - 96 -- a-z are 1-26, ord 'a' = 97
    priority' c = (c - 64) + 26 -- A-Z are 27-52, ord 'A' = 65

compartmentalise :: [Char] -> ([Char], [Char])
compartmentalise x = splitAt (compartmentSize x) x
  where
    compartmentSize = (`div` 2) . length

inBoth :: [Char] -> [Char]
inBoth = nub . uncurry intersect . compartmentalise

costInBoth :: [Char] -> Int
costInBoth = sum . map priority . inBoth

elfGroups :: [String] -> [[String]]
elfGroups = chunksOf 3

badgeItem :: [String] -> Char
badgeItem (x:y:z:_) = first (intersect x (intersect y z))
  where
    first (x:_) = x
    first _     = ' '

part1 :: String -> IO Int
part1 = ((sum . map costInBoth . lines) <$>) . readFile

part2 :: String -> IO Int
part2 = ((sum . map (priority . badgeItem) . elfGroups . lines) <$>) . readFile
